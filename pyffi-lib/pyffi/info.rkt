#lang info

(define deps       (list "base" "at-exp-lib"))
(define build-deps (list "base" "at-exp-lib"))

(define raco-commands
  (list (list "pyffi" 'pyffi/configure-pyffi "configure pyffi" #f)))

;; configure-pyffi.rkt is a `raco pyffi` command-line script: it
;; dispatches via `command-line` at the top level so `raco pyffi
;; <subcommand>` works.  Without this exclusion, `raco test` (which
;; the pkg-build server runs over every .rkt in the package) would
;; load it as a script with no extra argv, fall through to the
;; usage-fallback `(exit 3)` branch, and report a spurious test
;; failure.
(define test-omit-paths
  '("configure-pyffi.rkt"))
