#lang racket/base

;;; Demonstration test for Racket → Python callbacks.
;;;
;;; Each test exercises something that was previously impossible:
;;; passing a Racket procedure into Python where Python expects a
;;; callable.  Higher-order builtins (`map`, `filter`, `sorted`),
;;; library hooks, plugin architectures.

(require pyffi
         pyffi/structs
         pyffi/python-c-api
         pyffi/python-initialization
         pyffi/python-types
         pyffi/python-string
         pyffi/python-callback
         pyffi/python-exception
         rackunit)

(initialize)
(finish-initialization)

(define (catching pred thunk)
  (with-handlers ([pred values]) (thunk) #f))

;; Deep-convert a returned Python value (often arriving as an obj
;; wrapper for compound types) into ordinary Racket data.  Pyffi's
;; `pr` only handles atomic types and wraps everything else as an
;; obj; for lists and strings we recurse / use the dedicated
;; converters.
(require pyffi/python-list)
(define (->racket v)
  (cond
    [(or (number? v) (boolean? v) (void? v) (string? v) (null? v))  v]
    [(list? v)  (map ->racket v)]
    [(obj? v)
     (case (obj-type-name v)
       [("str")   (py-string->string (obj-the-obj v))]
       [("int")   (py-int->number   (obj-the-obj v))]
       [("float") (py-float->number (obj-the-obj v))]
       [("bool")  (= 1 (PyObject_IsTrue (obj-the-obj v)))]
       [("NoneType") (void)]
       [("list")  (map ->racket (pylist->list v))]
       [else      v])]
    [else v]))

;; ===================================================================
;; 1. Pass a Racket predicate to Python's `filter` [PREVIOUSLY IMPOSSIBLE]
;;
;; `filter` is one of Python's higher-order builtins; in pyffi-lib's
;; python-builtins.rkt it sat behind a TODO ("callbacks?").  With a
;; real callback bridge, a Racket predicate is just a Python callable.
;; ===================================================================

(test-case "[NEW] filter with a Racket predicate"
  (run* "values = [1, -2, 3, -4, 5, -6]")
  (define keep-positive? (racket-procedure->python positive?))
  (run* "
def _apply_filter(pred, xs):
    return list(filter(pred, xs))
")
  (define apply-filter (run "_apply_filter"))
  (define out (apply-filter keep-positive? (run "values")))
  (check-equal? (->racket out) '(1 3 5)))

;; ===================================================================
;; 2. Pass a Racket key function to `sorted` [PREVIOUSLY IMPOSSIBLE]
;;
;; sorted(key=…) is the canonical example of a Python builtin that
;; takes a callable that returns a comparison key.
;; ===================================================================

(test-case "[NEW] sorted with a Racket key function"
  (run* "names = ['Charlie', 'alice', 'Bob']")
  (define lower-key (racket-procedure->python
                     (λ (s) (string-downcase s))))
  (run* "
def _apply_sorted(key, xs):
    return sorted(xs, key=key)
")
  (define apply-sorted (run "_apply_sorted"))
  (define out (apply-sorted lower-key (run "names")))
  (check-equal? (->racket out) '("alice" "Bob" "Charlie")))

;; ===================================================================
;; 3. Use a Racket function as a `map` callback [PREVIOUSLY IMPOSSIBLE]
;;
;; map() returns a Python iterator over results of calling the
;; callable on each input element.
;; ===================================================================

(test-case "[NEW] map with a Racket callable"
  (run* "
def _apply_map(fn, xs):
    return list(map(fn, xs))
")
  (define square (racket-procedure->python (λ (x) (* x x))))
  (define out ((run "_apply_map") square (run "[1, 2, 3, 4, 5]")))
  (check-equal? (->racket out) '(1 4 9 16 25)))

;; ===================================================================
;; 4. Pass-through round-trip — Racket calls Python which calls
;;    Racket which calls Python.  Verifies the GIL-held / re-entrant
;;    case works without deadlock.
;; ===================================================================

(test-case "[NEW] re-entrant Racket → Python → Racket → Python"
  ;; Racket fn that, given a Python list, returns its length+1 by
  ;; calling Python's len() on it from inside Racket.
  (define len+1
    (racket-procedure->python
     (λ (xs)
       (define plen (run "len"))
       (+ 1 (pr (plen xs))))))
  (run* "
def _entry(callback):
    return callback([10, 20, 30])
")
  (define out ((run "_entry") len+1))
  (check-equal? (->racket out) 4))

;; ===================================================================
;; 5. A Racket exception in a callback surfaces as a Python
;;    RuntimeError (catchable on the Python side).
;; ===================================================================

(test-case "[NEW] Racket exn in callback becomes Python RuntimeError"
  (define always-throws
    (racket-procedure->python
     (λ (_) (error 'callback "racket-side blow-up"))))
  (run* "
def _try(cb):
    try:
        cb(0)
        return 'no-throw'
    except RuntimeError as e:
        return ('caught: ' + str(e))
")
  (define out ((run "_try") always-throws))
  (check-true (regexp-match? #rx"racket-side blow-up" (->racket out))))

;; ===================================================================
;; 6. A Racket re-raise of a Python exception (raise-into-python)
;;    surfaces with the typed Python class — useful for callbacks
;;    that want to translate Racket failures into idiomatic Python
;;    exception types.
;; ===================================================================

(test-case "[NEW] raise-into-python from inside a callback"
  (define type-error-class (run "TypeError"))
  (define raises-type-error
    (racket-procedure->python
     (λ (_)
       (raise-into-python type-error-class
                          #:value (run "TypeError('callback rejected the value')"))
       ;; The pending error indicator surfaces on the way out;
       ;; returning a Racket value is irrelevant.
       (void))))
  (run* "
def _try(cb):
    try:
        cb(0)
        return 'no-throw'
    except TypeError as e:
        return 'caught:' + str(e)
")
  (define out ((run "_try") raises-type-error))
  (check-equal? (->racket out) "caught:callback rejected the value"))

;; ===================================================================
;; 7. Multiple wrapped callables coexist independently — each
;;    capsule routes to its own Racket procedure.
;; ===================================================================

(test-case "[NEW] multiple callbacks coexist"
  (define inc  (racket-procedure->python (λ (x) (+ x 1))))
  (define dbl  (racket-procedure->python (λ (x) (* x 2))))
  (define neg  (racket-procedure->python (λ (x) (- x))))
  (run* "
def _three(a, b, c):
    return [a(10), b(10), c(10)]
")
  (define out ((run "_three") inc dbl neg))
  (check-equal? (->racket out) '(11 20 -10)))

;; ===================================================================
;; 8. The wrapped callable's __name__ and __doc__ are honoured.
;; ===================================================================

(test-case "[NEW] __name__ and __doc__ propagate"
  (define cb (racket-procedure->python (λ (x) x)
                                       #:name "identity"
                                       #:doc  "Returns its argument unchanged."))
  (run* "
def _read_meta(fn):
    return [fn.__name__, fn.__doc__]
")
  (define out ((run "_read_meta") cb))
  (check-equal? (->racket out) '("identity" "Returns its argument unchanged.")))

;; ===================================================================
;; 9. A non-exn Racket raise does NOT crash the trampoline
;;    [REGRESSION TEST for reviewer concern #3]
;;
;; The original handlers caught only python-exception? and exn:fail?.
;; A Racket procedure that did `(raise 'something)` (a non-exn value)
;; or threw an exn:break would have unwound the stack through the C
;; trampoline frame, which is undefined behaviour on the CPython side.
;; The catch-all clause must turn any other thrown value into a
;; Python RuntimeError so Python observes a clean failed call.
;; ===================================================================

(test-case "[REGRESSION] non-exn Racket raise becomes a Python RuntimeError"
  (define throws-symbol
    (racket-procedure->python
     (λ (_) (raise 'symbol-as-error))))
  (run* "
def _try(cb):
    try:
        cb(0)
        return 'no-throw'
    except RuntimeError as e:
        return 'caught:' + str(e)
")
  (define out ((run "_try") throws-symbol))
  (check-true (regexp-match? #rx"symbol-as-error" (->racket out))
              (format "expected RuntimeError text containing the raised value, got ~v"
                      (->racket out))))

(test-case "[REGRESSION] non-exn Racket raise of an integer becomes a Python RuntimeError"
  (define throws-int
    (racket-procedure->python
     (λ (_) (raise 99))))
  (run* "
def _try(cb):
    try:
        cb(0)
        return 'no-throw'
    except RuntimeError as e:
        return 'caught:' + str(e)
")
  (define out ((run "_try") throws-int))
  (check-true (regexp-match? #rx"99" (->racket out))))

;; ===================================================================
;; 10. ml_name / ml_doc survive Racket GC pressure
;;     [REGRESSION TEST for reviewer concern #4]
;;
;; Originally ml_name / ml_doc were assigned the result of
;; string->bytes/utf-8 -- 3m-GC'd bytes objects.  3m relocates objects
;; on collection, so a pointer into a Racket bytes inside a raw
;; PyMethodDef could dangle as soon as the GC ran.  The fix copies
;; the strings into raw-malloc'd C buffers freed by the capsule
;; destructor.  We exercise this by allocating the wrapper, then
;; provoking several major collections plus allocation pressure, and
;; finally reading __name__ and __doc__ back through Python.
;; ===================================================================

(test-case "[REGRESSION] ml_name / ml_doc survive Racket GC"
  (define cb
    (racket-procedure->python (λ (x) x)
                              #:name "stress-test-name"
                              #:doc  "Docstring that must survive GC."))
  (run* "
def _read_meta(fn):
    return [fn.__name__, fn.__doc__]
")
  ;; Provoke major collections plus a generous burst of fresh bytes
  ;; allocations.  Under the original code the bytes backing ml_name
  ;; and ml_doc could be relocated or reclaimed on a major GC, so a
  ;; subsequent read of fn.__name__ would return garbage or segfault.
  (for ([_ (in-range 8)])
    (collect-garbage 'major))
  (for ([_ (in-range 20000)])
    (string->bytes/utf-8
     (number->string (random 1000000))))
  (for ([_ (in-range 8)])
    (collect-garbage 'major))

  (define out ((run "_read_meta") cb))
  (check-equal? (->racket out)
                '("stress-test-name" "Docstring that must survive GC.")))
