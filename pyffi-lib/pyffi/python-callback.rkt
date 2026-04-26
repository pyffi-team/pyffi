#lang racket/base

;;; Wrap a Racket procedure as a Python callable.
;;;
;;; Without this, Racket can call Python but Python cannot call back.
;;; That cuts off whole categories of practical Python usage —
;;; higher-order builtins (`map`, `filter`, `sorted(key=…)`, `reduce`),
;;; asyncio, GUI toolkits, library hooks (pandas `apply`, JSON
;;; `default=`, scikit-learn custom metrics, …), and any plugin
;;; architecture where Python expects to register callbacks.
;;;
;;; The mechanism is the standard CPython recipe for exposing a C
;;; function as a Python callable: a single C-level trampoline whose
;;; per-call state is delivered via the `self` parameter as a
;;; PyCapsule.  Each `racket-procedure->python` invocation interns
;;; the Racket procedure in a per-module hash, builds a capsule
;;; carrying its integer key, and constructs a Python function
;;; whose ml_meth points at the trampoline.  When Python calls the
;;; resulting function, the trampoline reaches into the hash via
;;; the capsule, converts the args tuple into Racket values, calls
;;; the procedure, converts the result back, and translates any
;;; raised Racket exception into a typed Python exception.
;;;
;;; Lifetime: the Racket procedure stays alive as long as the
;;; PyCapsule exists, because the hash entry is rooted by the
;;; capsule destructor.  When Python lets go of the wrapper, the
;;; capsule is GC'd, the destructor fires, the hash entry is
;;; removed, and the Racket procedure becomes eligible for Racket-
;;; side GC.

(require (except-in ffi/unsafe ->)
         racket/match
         "structs.rkt"
         "python-c-api.rkt"
         (except-in "python-types.rkt" cast)
         "python-exception.rkt")

(provide racket-procedure->python
         (rename-out [racket-procedure->python py-lambda]))

;; ------------------------------------------------------------------
;; Per-module Racket-procedure registry
;;
;; We can't put a Racket procedure pointer into a PyCapsule directly
;; — Racket's GC may move it.  Instead we intern the procedure in
;; this hashtable, indexed by a fresh integer key, and store the
;; integer in the capsule.  The trampoline reads the integer back
;; out and looks the procedure up.
;;
;; The capsule destructor removes the entry, releasing the Racket-
;; side root and letting the procedure be collected.
;; ------------------------------------------------------------------

(define proc-table (make-hasheqv))
;; Start at 1: PyCapsule_New rejects a NULL pointer payload, and a
;; key of 0 casts to NULL.
(define next-key 1)

(define (intern-proc proc)
  (define k next-key)
  (set! next-key (+ next-key 1))
  (hash-set! proc-table k proc)
  k)

(define (lookup-proc k)
  (hash-ref proc-table k #f))

(define (release-proc k)
  (hash-remove! proc-table k))

;; Cast helpers between integer keys and the void* pointer slot in
;; a PyCapsule.  The cast is safe: a capsule pointer is treated as
;; opaque by Python, and our key is always an exact non-negative
;; integer well within intptr range.
(define (key->pointer k) (cast k _intptr _pointer))
(define (pointer->key p) (cast p _pointer _intptr))

;; ------------------------------------------------------------------
;; Trampoline: the single C-callable function used by every wrapped
;; Racket procedure.  Per-call state arrives in `self` (the capsule)
;; and `args` (the positional tuple).
;; ------------------------------------------------------------------

(define (trampoline-impl self args)
  (define key  (pointer->key (PyCapsule_GetPointer self #f)))
  (define proc (lookup-proc key))
  (cond
    [(not proc)
     ;; Stale capsule (procedure was released while a wrapper was
     ;; still in flight).  Tell Python; return NULL.
     (PyErr_SetString PyExc_RuntimeError
                      "racket-procedure->python: callable invoked after release")
     #f]
    [else
     (with-handlers
       ;; A Python exception caught earlier in Racket and surfaced
       ;; as an exn:fail:pyffi:python re-enters Python losslessly,
       ;; preserving identity, traceback and chained context.
       ([python-exception?
         (λ (e) (reraise-into-python e) #f)]
        ;; Any other Racket failure becomes a Python RuntimeError
        ;; carrying the Racket exn message.  `raise-into-python`
        ;; sets the indicator; returning #f tells Python the call
        ;; failed.
        [exn:fail?
         (λ (e)
           (PyErr_SetString PyExc_RuntimeError (exn-message e))
           #f)])
       ;; `python->racket` is the deeper converter: it converts
       ;; Python str → Racket string, tuple → vector, etc., where
       ;; `pr` would wrap them as `obj`s.  We use the deep one so
       ;; Racket callbacks see ordinary Racket values for the common
       ;; atomic and string cases without callers having to unwrap.
       (define racket-args
         (let loop ([i 0] [n (PyTuple_Size args)] [acc '()])
           (if (= i n)
               (reverse acc)
               (loop (+ i 1) n (cons (python->racket (PyTuple_GetItem args i)) acc)))))
       (define result (apply proc racket-args))
       ;; If the Racket procedure called `raise-into-python` (or
       ;; otherwise set the Python error indicator and returned
       ;; normally), Python expects us to return NULL — returning
       ;; both a value and a pending indicator is a SystemError.
       (cond
         [(PyErr_Occurred) #f]
         [else
          (define result-ptr (rp result))
          ;; PyCFunction return values are owned by the caller —
          ;; Python decref's exactly once at the end of the call.
          ;; When `rp` returns a fresh Python object
          ;; (`string->py-string`, `list->py-list`, …) it already
          ;; has refcount=1 and the ownership transfer is balanced.
          ;; When the Racket procedure passes through an `obj` it
          ;; received as input, `rp` hands back a borrowed pointer;
          ;; without an extra Py_IncRef the underlying object would
          ;; be freed prematurely and any surviving Racket-side
          ;; `obj` wrapper would dangle.
          (when (obj? result) (Py_IncRef result-ptr))
          result-ptr]))]))

(define trampoline-fnptr
  ;; Convert the Racket implementation into a real C-callable
  ;; function pointer that pyfunction objects can store in
  ;; ml_meth.  This pointer lives forever — we want exactly one
  ;; trampoline shared across every wrapped procedure.
  (function-ptr trampoline-impl
                (_fun _PyObject* _PyObject* -> _PyObject*)))

;; ------------------------------------------------------------------
;; Capsule destructor
;;
;; Fires when Python finalises the capsule (which it does when the
;; wrapping function object becomes garbage).  The destructor reads
;; the integer key out of the capsule and releases the entry from
;; the proc-table so the Racket procedure can be collected.
;; ------------------------------------------------------------------

(define (capsule-destructor-impl capsule)
  (define key (pointer->key (PyCapsule_GetPointer capsule #f)))
  (release-proc key))

(define capsule-destructor-fnptr
  (function-ptr capsule-destructor-impl
                (_fun _PyObject* -> _void)))

;; ------------------------------------------------------------------
;; Per-callable PyMethodDef storage
;;
;; PyCFunction_NewEx takes a pointer to a PyMethodDef and assumes
;; it remains live for the lifetime of the resulting Python
;; function.  We malloc one per wrapping (raw, not GC'd) and free
;; it when the capsule destructor fires.
;; ------------------------------------------------------------------

;; Hash so the malloc'd PyMethodDef pointers stay reachable from
;; Racket's perspective for as long as Python could still call
;; the trampoline.  Indexed by the same integer key.  Cleared by
;; the capsule destructor at the same time as proc-table.
(define methoddef-table (make-hasheqv))

(define (intern-methoddef key md) (hash-set! methoddef-table key md))
(define (release-methoddef key)
  (define md (hash-ref methoddef-table key #f))
  (when md
    (free md)
    (hash-remove! methoddef-table key)))

;; Wrap the public destructor so it also frees the PyMethodDef.
(define (capsule-destructor-impl/full capsule)
  (define key (pointer->key (PyCapsule_GetPointer capsule #f)))
  (release-proc key)
  (release-methoddef key))

(define capsule-destructor-fnptr/full
  (function-ptr capsule-destructor-impl/full
                (_fun _PyObject* -> _void)))

;; ------------------------------------------------------------------
;; Public constructor
;; ------------------------------------------------------------------

;; Wrap a Racket procedure as a Python callable.
;;
;;   proc    — a Racket procedure of arbitrary arity.
;;   #:name  — Python __name__ attribute (default "racket-procedure").
;;   #:doc   — Python __doc__ attribute (default #f → no docstring).
;;
;; Returns an `obj` wrapping a Python function.  Calling it from
;; Python invokes `proc` with the converted positional arguments;
;; the returned Python value is the converted Racket result.
;; Exceptions raised by `proc` surface to Python: a previously
;; caught `python-exception?` round-trips losslessly via
;; `reraise-into-python`; any other `exn:fail?` becomes a Python
;; RuntimeError carrying the Racket exn message.
(define (racket-procedure->python proc
                                  #:name [name "racket-procedure"]
                                  #:doc  [doc  #f])
  (unless (procedure? proc)
    (raise-argument-error 'racket-procedure->python "procedure?" proc))
  (define key (intern-proc proc))
  ;; Allocate and populate a PyMethodDef.  Raw-malloc'd: lives until
  ;; the capsule destructor fires.
  (define md (malloc _PyMethodDef 'raw))
  (cpointer-push-tag! md PyMethodDef-tag)
  (set-PyMethodDef-ml_name!  md (string->bytes/utf-8 name))
  (set-PyMethodDef-ml_meth!  md trampoline-fnptr)
  (set-PyMethodDef-ml_flags! md METH_VARARGS)
  (set-PyMethodDef-ml_doc!   md (and doc (string->bytes/utf-8 doc)))
  (intern-methoddef key md)
  ;; Box the integer key in a capsule and attach the destructor.
  (define capsule
    (PyCapsule_New (key->pointer key) #f capsule-destructor-fnptr/full))
  ;; Build the Python function object.  `module` is NULL — these
  ;; aren't methods of any importable module.
  (define fn (PyCFunction_NewEx md capsule #f))
  (obj "function" fn))
