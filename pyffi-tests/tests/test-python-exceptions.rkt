#lang racket/base

;;; Demonstration test for the new typed Python-exception surface in
;;; pyffi.  Each test exercises something that under the previous
;;; design (a bare `(exn …)` carrying only a formatted message) was
;;; either impossible, broken, or required string-matching against
;;; the message.
;;;
;;; The headings of each test case identify what's NEW vs what was
;;; possible before.

;; `pyffi` (the umbrella module) re-exports the *wrapped* `run` /
;; `run*` from python.rkt — the ones that go through
;; `handle-python-exception` and turn Python errors into typed
;; Racket exceptions.  Importing `pyffi/python-evaluation` directly
;; gives you the raw `run` that returns `#f` on Python error without
;; raising, which is not what we want to exercise here.
(require pyffi
         pyffi/structs
         pyffi/python-c-api
         pyffi/python-initialization
         pyffi/python-types
         pyffi/python-string
         pyffi/python-exception
         rackunit)

(initialize)
(finish-initialization)

(define (catching pred thunk)
  (with-handlers ([pred values]) (thunk) #f))

;; ===================================================================
;; 1. exn:fail? actually catches Python exceptions [PREVIOUSLY BROKEN]
;;
;; Before: pyffi raised a bare `(exn …)`, the abstract base type.
;; A standard `(with-handlers ([exn:fail? …]) …)` did NOT catch it,
;; even though every other Racket library raises exn:fail subtypes
;; for recoverable failures.  Users had to fall back to `exn?`,
;; which over-catches — it also catches breaks.
;;
;; Now: exn:fail:pyffi:python is a subtype of exn:fail.
;; ===================================================================

(test-case "[NEW] exn:fail? catches Python exceptions"
  (define caught (catching exn:fail? (λ () (run "1/0"))))
  (check-pred exn:fail? caught)
  (check-pred python-exception? caught))

;; ===================================================================
;; 2. Dispatch on Python class WITHOUT parsing the message [NEW]
;;
;; Before: only the formatted message string was available.  Code
;; that wanted to "catch ValueError but not KeyError" had to do
;;   (regexp-match? #rx"^.*: ValueError" (exn-message e))
;; which is fragile across Python versions, message formats, and
;; locales.
;;
;; Now: `python-exception-type-name` returns the Python class name
;; as structured data.  `python-exception-class` exposes the live
;; class object for `py-isinstance?` against other Python types.
;; ===================================================================

(test-case "[NEW] dispatch by Python class name without message parsing"
  (define value-err (catching python-exception?
                              (λ () (run "int('not a number')"))))
  (check-equal? (python-exception-type-name value-err) "ValueError")

  (define key-err   (catching python-exception?
                              (λ () (run "{}['missing']"))))
  (check-equal? (python-exception-type-name key-err) "KeyError")

  ;; Custom user-defined exception class flows through identically.
  (run* "class MyError(ValueError): pass")
  (define custom    (catching python-exception?
                              (λ () (run* "raise MyError('boom')"))))
  (check-equal? (python-exception-type-name custom) "MyError"))

;; ===================================================================
;; 3. Reach the live Python exception object [NEW]
;;
;; Before: the Python value was discarded after formatting.  Custom
;; attributes the raiser had attached to the exception (`e.errno`,
;; `e.filename`, app-specific fields) were unreachable from Racket
;; — they were lost the moment the exception entered the formatter.
;;
;; Now: `python-exception-value` returns the live `obj` wrapping the
;; Python exception instance.  Every attribute pure Python could see
;; is accessible.
;; ===================================================================

(test-case "[NEW] reach custom attributes the Python raiser attached"
  (run* "
class CustomError(Exception):
    def __init__(self, msg, errno, host):
        super().__init__(msg)
        self.errno = errno
        self.host  = host
")
  (define e (catching python-exception?
                      (λ () (run* "raise CustomError('connection lost', 42, 'db.example.com')"))))
  (define value (obj-the-obj (python-exception-value e)))

  (define errno-obj (PyObject_GetAttrString value "errno"))
  (check-equal? (PyLong_AsLong errno-obj) 42)

  (define host-obj  (PyObject_GetAttrString value "host"))
  (check-equal? (PyUnicode_AsUTF8 host-obj) "db.example.com"))

;; ===================================================================
;; 4. Walk the chained `__cause__` graph (`raise X from Y`) [NEW]
;;
;; Before: chained context only existed in the formatted traceback
;; as text.  Programmatically distinguishing "what raised X" from
;; "what X was caused by" required parsing.
;;
;; Now: `__cause__` and `__context__` are normal attributes of the
;; live value; pyffi consumers reach them with the standard
;; attribute-lookup machinery.
;; ===================================================================

(test-case "[NEW] chained __cause__ accessible programmatically"
  (run* "
def outer():
    try:
        int('abc')
    except ValueError as v:
        raise RuntimeError('outer failed') from v
")
  (define e (catching python-exception? (λ () (run "outer()"))))
  (check-equal? (python-exception-type-name e) "RuntimeError")

  (define value (obj-the-obj (python-exception-value e)))
  (define cause (PyObject_GetAttrString value "__cause__"))
  (define cause-name
    (PyUnicode_AsUTF8 (PyObject_GetAttrString (PyObject_Type cause) "__name__")))
  (check-equal? cause-name "ValueError"))

;; ===================================================================
;; 5. Walk the LIVE traceback object frame-by-frame [NEW]
;;
;; Before: traceback was pre-baked as formatted strings.  Walking
;; frames programmatically (file, line, function, locals) was not
;; possible from the Racket side.
;;
;; Now: `value.__traceback__` is the live Python traceback object;
;; the standard tb_frame / tb_lineno / tb_next chain is reachable.
;; ===================================================================

(test-case "[NEW] walk the live Python traceback object"
  (run* "
def inner():
    1/0

def outer():
    inner()
")
  (define e (catching python-exception? (λ () (run "outer()"))))
  (define value (obj-the-obj (python-exception-value e)))
  (define tb0   (PyObject_GetAttrString value "__traceback__"))

  (define names
    (let loop ([tb tb0] [acc '()])
      (cond
        [(not tb)             (reverse acc)]
        [(equal? tb the-None) (reverse acc)]
        [else
         (define frame (PyObject_GetAttrString tb "tb_frame"))
         (define code  (PyObject_GetAttrString frame "f_code"))
         (define fname (PyUnicode_AsUTF8
                        (PyObject_GetAttrString code "co_name")))
         (loop (PyObject_GetAttrString tb "tb_next") (cons fname acc))])))

  (check-true (and (member "outer" names) #t) "outer in traceback")
  (check-true (and (member "inner" names) #t) "inner in traceback"))

;; ===================================================================
;; 6. KeyboardInterrupt is NOT caught by exn:fail? [PREVIOUSLY BROKEN]
;;
;; Before: every Python exception (including KeyboardInterrupt)
;; arrived as the same bare `(exn …)`, so a broad
;;   (with-handlers ([exn:fail? swallow]) …)
;; silently swallowed user interrupts that came from Python.
;;
;; Now: KeyboardInterrupt becomes `exn:break:pyffi:python`, a
;; subtype of `exn:break`.  Caught by `exn:break?` and `exn?` but
;; NOT by `exn:fail?`.
;; ===================================================================

(test-case "[NEW] KeyboardInterrupt becomes exn:break, not exn:fail"
  (define caught
    (catching exn:break? (λ () (run* "raise KeyboardInterrupt('user pressed Ctrl-C')"))))
  (check-pred exn:break:pyffi:python? caught)
  (check-pred python-exception? caught)
  (check-equal? (python-exception-type-name caught) "KeyboardInterrupt")

  ;; Demonstrate that `exn:fail?` does NOT match: we set up dual
  ;; handlers; the break handler must be the one that fires.
  (define classification
    (with-handlers ([exn:break? (λ (_) 'break)]
                    [exn:fail?  (λ (_) 'fail)])
      (run* "raise KeyboardInterrupt")
      'completed))
  (check-equal? classification 'break))

;; ===================================================================
;; 7. SystemExit is honoured as a real exit [NEW]
;;
;; Before: a Python `sys.exit(7)` reached the Racket caller as a
;; bare exn with a stringified "SystemExit: 7" message; the embedder
;; had no clean way to obey the request.
;;
;; Now: SystemExit is intercepted in `handle-python-exception` and
;; turned into a Racket `(exit code)` directly, mirroring how Python
;; embeddings conventionally honour sys.exit.
;;
;; (We don't actually exit during the test suite; we use a
;; subprocess to demonstrate.)
;; ===================================================================

(test-case "[NEW] SystemExit honoured (subprocess demo)"
  ;; Spawn a Racket subprocess that runs sys.exit(42) via pyffi's
  ;; wrapped `run*` and verify the subprocess exits with code 42.
  (define cmd
    (string-append
     "racket -e '"
     "(require pyffi pyffi/python-initialization)"
     "(initialize)(finish-initialization)"
     "(run* \"import sys\")"
     "(run* \"sys.exit(42)\")'"))
  (define-values (sp _in _out _err)
    (subprocess #f #f (current-error-port) "/bin/sh" "-c" cmd))
  (subprocess-wait sp)
  (check-equal? (subprocess-status sp) 42))

;; ===================================================================
;; 8. python-exception? unifies the predicate surface [NEW]
;;
;; Before: there was no single predicate that meant "Python raised".
;; You either used `exn?` (catches everything, including Racket
;; errors) or string-matched the message.
;;
;; Now: `python-exception?` matches both flavours of pyffi's typed
;; Python exceptions and only those.  The test includes a positive
;; case for each flavour and a negative case for a Racket-side error.
;; ===================================================================

(test-case "[NEW] python-exception? is the unified Python-source predicate"
  (check-pred python-exception?
              (catching python-exception? (λ () (run "1/0"))))
  (check-pred python-exception?
              (catching python-exception? (λ () (run* "raise KeyboardInterrupt"))))

  (define racket-err (catching exn:fail?
                               (λ () (error 'test "racket-side error"))))
  (check-false (python-exception? racket-err)))

;; ===================================================================
;; 9. Round-trip back into Python with full identity [NEW]
;;
;; Before: not possible.  Once an exception entered the formatter
;; the live Python object was unrecoverable.
;;
;; Now: `reraise-into-python` re-installs the same Python object in
;; Python's error indicator.  Downstream Python code that catches it
;; sees the same object identity (`is`-check passes) and can
;; introspect every attribute — exactly as if Racket had never
;; intercepted.
;; ===================================================================

(test-case "[NEW] reraise-into-python preserves Python identity through the round trip"
  (run* "
class Sentinel(Exception):
    def __init__(self, tag):
        super().__init__(tag)
        self.tag = tag
")
  ;; First catch.
  (define caught (catching python-exception? (λ () (run* "raise Sentinel('round-trip')"))))
  (check-equal? (python-exception-type-name caught) "Sentinel")
  (define before-ptr (obj-the-obj (python-exception-value caught)))

  ;; Re-install the original Python exception in the indicator and
  ;; let pyffi pick it up on the next Python call.  The wrapped run
  ;; sees the indicator and re-raises into Racket as a typed exn —
  ;; meaning a Python try/except *inside the same call* would also
  ;; have caught it the moment Python entered its main loop.
  (reraise-into-python caught)
  (define recaught (catching python-exception? (λ () (run "1"))))

  ;; Same Python class name on the other side of the trip.
  (check-equal? (python-exception-type-name recaught) "Sentinel")

  ;; And — losslessness check — the live Python object handed back is
  ;; the SAME object Python originally raised.  Identity preserved
  ;; through the indicator round trip: `eq?` on the underlying
  ;; pointer.
  (define after-ptr (obj-the-obj (python-exception-value recaught)))
  (check-equal? after-ptr before-ptr "live Python object identity preserved")

  ;; And tag (the custom attribute) is still readable.
  (check-equal? (PyUnicode_AsUTF8 (PyObject_GetAttrString after-ptr "tag")) "round-trip"))

;; ===================================================================
;; 10. Inject a Racket-side failure as a typed Python exception [NEW]
;;
;; Before: a Racket-side error occurring during a Python callback
;; surfaced as a confusing embedding error in Python, with no class
;; the Python code could meaningfully catch.
;;
;; Now: `raise-into-python` lets Racket present a domain-specific
;; Python exception class.  Python code can then write idiomatic
;; `except TypeError:` (or any other class) and catch it cleanly.
;; ===================================================================

(test-case "[NEW] raise-into-python installs a typed Python exception"
  (define type-error-class (run "TypeError"))

  ;; raise-into-python sets the Python error indicator with a fresh
  ;; TypeError carrying our message.  The next pyffi call surfaces
  ;; the indicator: pyffi's wrapped `run` re-raises it into Racket
  ;; as exn:fail:pyffi:python with type-name "TypeError".  This
  ;; demonstrates that a Racket-side raise-into-python becomes
  ;; visible to Python (via the indicator) with the class identity
  ;; the caller picked.
  (raise-into-python type-error-class
                     #:value (run "TypeError('Racket said no')"))
  (define surfaced (catching python-exception? (λ () (run "1"))))

  (check-equal? (python-exception-type-name surfaced) "TypeError")
  (check-true (regexp-match? #rx"Racket said no" (exn-message surfaced))))
