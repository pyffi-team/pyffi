#lang at-exp racket/base

;;;  This module loads the shared library `libpython` and
;;;  provides the form `define-python` which is used to
;;;  create bindings for Python's C-API.

(provide define-python
         pyffi-natipkg-root)

;;; Imports

(require ffi/unsafe ffi/unsafe/define racket/file racket/list racket/promise
         pkg/lib)


;;; Configuration

(define libpython-folder (get-preference 'pyffi:libdir (λ () #f)))
#;(unless libpython-folder
  (parameterize ([current-output-port (current-error-port)])
    (displayln "There is no preference for 'pyffi:libdir' set.")
    (displayln "In order for `pyffi` to find the shared library `libpython3` (or `libpython3.10`) ")
    (displayln "you must set the 'pyffi:libdir' preference to the folder of the shared library.")
    (displayln "The most convenient way to do this, is to run `raco pyffi configure`.")
    (displayln "See details in the documentation.")
    (exit 1)))

(define extension
  (case (system-type 'os)
    [(macosx)  "dylib"]
    [(unix)    "so"]
    [(windows) "dll"]
    [else      (error 'internal-error:extension "File a bug report on Github.")]))


;; find-libpython3 : (or/c path-string? #f) -> (or/c path? #f)
;;   If libpython-folder is a directory, return the path to a libpython
;;   file inside it, or #f if none is found or folder is #f.
;;
;;   Filenames matched, by platform:
;;     macOS  : libpython3.<X>[abi].dylib
;;     Linux  : libpython3.<X>[abi].so[.<N>[.<M>...]]
;;     Windows: libpython3.<X>[abi].dll           (rare — usually pythonXY.dll)
;;
;;   Where <X> is one or more digits (so Python 3.9 through 3.99+ all work)
;;   and [abi] is an optional Python ABI flag letter such as `t`
;;   (free-threaded, 3.13+), `d` (debug), or `m` (historical).
;;
;;   On Linux this accepts both the unversioned `.so` symlink and the
;;   real SONAME variants like `libpython3.12.so.1.0`.  This matters on
;;   distros that no longer ship the unversioned symlink in
;;   libpython3-dev (Ubuntu 24.04 onward).
(define (find-libpython3 libpython-folder)
  (cond
    [(not libpython-folder) #f]
    [else
     (define dir (path->complete-path libpython-folder))
     (define rx
       (case (system-type 'os)
         [(unix)
          ;; libpython3.12.so, libpython3.12.so.1, libpython3.12.so.1.0,
          ;; libpython3.13t.so ...
          #rx"^libpython3\\.[0-9]+[a-z]*\\.so(\\.[0-9]+)*$"]
         [(macosx)
          ;; libpython3.12.dylib, libpython3.13t.dylib
          #rx"^libpython3\\.[0-9]+[a-z]*\\.dylib$"]
         [(windows)
          #rx"^libpython3\\.[0-9]+[a-z]*\\.dll$"]
         [else
          ;; Fallback: platform-specific extension with multi-digit minor.
          (regexp (format "^libpython3\\.[0-9]+[a-z]*~a$"
                          (regexp-quote (string-append "." extension))))]))
     (with-handlers ([exn:fail? (λ (_e) #f)])
       (for/or ([p (in-list (directory-list dir))])
         (let-values ([(base name must-dir?) (split-path p)])
           (and name
                (let ([s (path->string name)])
                  (and (regexp-match? rx s)
                       (build-path libpython-folder p)))))))]))


(define (build-full-path name)
  (if libpython-folder
      (build-path libpython-folder
                  (string->path (string-append name "." extension)))
      (string->path (string-append name "." extension))))

;; --- temporarily disabled

;; (define libpython-path
;;   (or (find-libpython3 libpython-folder) ; An absolute, full path
;;       (for/first ([name '("libpython3" "libpython3.10" "libpython310"
;;                                        )]
;;                   #:when (file-exists? (build-full-path name)))
;;         (build-full-path name))
;;       ;; Github Action (Ubuntu)
;;       ;;   On Github Action the `raco pyffi configure` is run after
;;       ;;   the documentation is rendered, so we need to provide the version here.
;;       "libpython3.14"))  


;; ; Note: If the Python interpreter loads a shared library dynamically,
;; ;       it needs access to the Python C-API. To make the symbols
;; ;       exported by a shared library visible to other shared libaries,
;; ;       we need to use a "flat namespace" and therefore use `#:global? #t`,
;; ;       when loading the library.

;; (require pyffi/parameters)
;; (define lib (ffi-lib libpython-path #:global? #t))

;; (define-ffi-definer define-python lib #:default-make-fail make-not-available)

;; --- temporarily disabled ends here


;;;
;;; Experimental lazy loading of libpython
;;;

;; --- libpython loading (runtime, lazy) -------------------------------------

;; Prefer an explicit env var, so CI/users can point to a specific libpython.
;; Examples:
;;   PYFFI_LIBPYTHON=/usr/lib/.../libpython3.12.so.1.0
;;   PYFFI_LIBPYTHON=libpython3.12
(define (env-libpython)
  (define s (getenv "PYFFI_LIBPYTHON"))
  (and s (not (string=? s "")) s))

(define (candidates)
  ;; Add more names if you like. Keep them as names (no extension);
  ;; build-full-path adds the platform extension when libpython-folder is set.
  '("libpython3"
    "libpython3.14" "libpython3.13" "libpython3.12" "libpython3.11" "libpython3.10"
    "libpython310"))

;; ---------------------------------------------------------------------------
;; natipkg auto-discovery
;;
;; When the user has neither `pyffi:libdir` nor `PYFFI_LIBPYTHON` set, look
;; for a sibling platform-specific natipkg package (`pyffi-<arch>-<os>-natipkg`)
;; that bundles a relocatable libpython plus its stdlib.  The natipkg
;; packages are installed automatically on matching platforms via pyffi-lib's
;; `platform-deps` declaration.  Each `define-runtime-path` below resolves at
;; module init against the installed package layout; if a natipkg isn't
;; installed, the resolved path points at a non-existent directory and is
;; filtered out by `directory-exists?`.
;; ---------------------------------------------------------------------------

(define (current-natipkg-name)
  (case (system-type 'os)
    [(unix)
     (case (system-type 'arch)
       [(aarch64) "pyffi-aarch64-linux-natipkg"]
       [(x86_64)  "pyffi-x86_64-linux-natipkg"]
       [else #f])]
    [(macosx)
     (case (system-type 'arch)
       [(aarch64) "pyffi-aarch64-macosx-natipkg"]
       [(x86_64)  "pyffi-x86_64-macosx-natipkg"]
       [else #f])]
    [else #f]))

(define (natipkg-lib-dir)
  ;; Locate the lib/ directory of the natipkg matching the current
  ;; host, or #f if the natipkg isn't installed or the platform isn't
  ;; covered.  `pkg-directory` returns #f cleanly for absent packages
  ;; (in contrast to `define-runtime-path` with a `'(lib …)` form,
  ;; which fails hard at compile time when the collection is missing).
  (define name (current-natipkg-name))
  (and name
       (with-handlers ([exn:fail? (λ (_) #f)])
         (define dir (pkg-directory name))
         (and dir
              (let ([lib (build-path dir "lib")])
                (and (directory-exists? lib) lib))))))

(define (natipkg-libpython)
  (define dir (natipkg-lib-dir))
  (and dir (find-libpython3 dir)))

;; Public: the natipkg's package root (the parent of its lib dir), or #f if
;; no natipkg is present.  python-initialization.rkt reads this to default
;; PYTHONHOME so Py_Initialize finds the bundled stdlib.
(define (pyffi-natipkg-root)
  (define lib (natipkg-lib-dir))
  (and lib
       (let-values ([(parent _ _2) (split-path lib)])
         (and (path? parent) parent))))

;; ---------------------------------------------------------------------------
;; Discovery order
;;
;;   1. PYFFI_LIBPYTHON      — explicit env-var override (highest priority)
;;   2. pyffi:libdir         — explicit user config (raco pyffi configure)
;;   3. natipkg auto-discovery — bundled libpython if a natipkg is installed
;;   4. dynamic loader search by name — last-resort default
;;   5. error                — nothing found
;; ---------------------------------------------------------------------------

(define (resolve-libpython-path)
  (cond
    [(env-libpython)
     (env-libpython)]
    [else
     (or (find-libpython3 libpython-folder)         ; user pref (pyffi:libdir)
         (for/first ([name (in-list (candidates))]  ; user pref by candidate name
                     #:when (file-exists? (build-full-path name)))
           (path->string (build-full-path name)))
         (let ([p (natipkg-libpython)])             ; bundled natipkg
           (and p (path->string p)))
         (for/first ([name (in-list (candidates))]) ; loader search by name
           name)
         (error 'pyffi
                (string-append
                 "Could not find/load libpython.\n"
                 "Try one of:\n"
                 " - Install the natipkg companion (e.g.\n"
                 "   `raco pkg install pyffi-aarch64-linux-natipkg`).\n"
                 " - Set preference 'pyffi:libdir' (raco pyffi configure).\n"
                 " - Set environment variable PYFFI_LIBPYTHON to a full path\n"
                 "   or a library name (e.g. libpython3.12).")))]))

;; Note: We use #:global? #t so that symbols are visible to extension modules.
;; IMPORTANT: Delay loading so that compilation/docs don't require libpython.
(define lib*
  (delay (ffi-lib (resolve-libpython-path) #:global? #t)))

(define (get-lib)
  (force lib*))

(require pyffi/parameters)

;; define-python is a macro, but the library expression is evaluated at runtime.
(define-ffi-definer define-python (get-lib)
  #:default-make-fail make-not-available)
