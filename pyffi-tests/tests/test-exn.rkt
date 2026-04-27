#lang racket

(require rackunit)

;; A renamed procedure that raises a plain exn must surface the
;; exn through the rename — the original message is preserved.
;; Asserting it keeps this file usable under raco test instead of
;; producing an unhandled raise at the top level.
(define foo
  (let ()
    (define proc (λ () (raise (exn "hello" (current-continuation-marks)))))
    (procedure-rename proc 'bar)))

(check-exn (λ (e) (and (exn? e) (string=? (exn-message e) "hello")))
           foo)
