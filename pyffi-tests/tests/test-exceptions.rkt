#lang racket

(require pyffi
         rackunit)
(initialize)
(finish-initialization)

(void (run* "def provoke0(baz=1): return 2/0"))
(define provoke0 (get-fun 'provoke0))

;; Calling provoke0 must surface ZeroDivisionError as the typed
;; exn:fail:pyffi:python.  Assert it instead of letting the
;; exception propagate to the top level (which would make raco
;; test report the file as failing).
(check-exn exn:fail:pyffi:python? (λ () (provoke0)))
