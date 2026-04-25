#lang info

(define collection 'multi)

(define deps '("base" "at-exp-lib"))

;; Platform-specific natipkg companions: each one bundles a
;; relocatable CPython 3.12 (libpython + stdlib) for one target.
;; They are *build-only* dependencies, not runtime dependencies.
;;
;; End users configure their own Python with `raco pyffi configure`
;; (or set the env var / preferences directly), so a normal install
;; does not download a natipkg.  The natipkg only exists to give
;; the package-build server a real libpython to link pyffi-doc's
;; Scribble examples against — without it the catalogue build of
;; pyffi-doc fails because the live examples can't import any
;; Python module.
;;
;; The build-server platform string is "x86_64-linux-natipkg"
;; specifically (distinct from "x86_64-linux"), so the second
;; entry below is the one that actually fires on pkg-build.  The
;; other three are there so a maintainer running `raco setup` on
;; another host (e.g. building docs locally before publishing) can
;; pick up the same fallback.
(define build-deps
  '("base" "at-exp-lib"
    ("pyffi-aarch64-linux-natipkg"  #:platform "aarch64-linux")
    ("pyffi-x86_64-linux-natipkg"   #:platform "x86_64-linux-natipkg")
    ("pyffi-aarch64-macosx-natipkg" #:platform "aarch64-macosx")
    ("pyffi-x86_64-macosx-natipkg"  #:platform "x86_64-macosx")))


(define pkg-desc "Use Python from Racket - Implementation part without documentation")

(define pkg-authors '(soegaard))

(define version "1.0")

(define test-responsibles '((all jensaxel@soegaard.net)))


