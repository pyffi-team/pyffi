#lang at-exp racket/base

;;;  This module loads the shared library `libpython` and
;;;  provides the form `define-python` which is used to 
;;;  create bindings for Python's C-API.

(provide define-python)

;;; Imports

(require ffi/unsafe ffi/unsafe/define racket/file racket/list racket/promise)


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

(define (resolve-libpython-path)
  (cond
    [(env-libpython)
     (env-libpython)]
    [else
     (or (find-libpython3 libpython-folder) ; full path inside libpython-folder
         (for/first ([name (in-list (candidates))]
                     #:when (file-exists? (build-full-path name)))
           (path->string (build-full-path name)))
         ;; If libpython-folder is not set, allow dynamic loader search by name:
         (for/first ([name (in-list (candidates))])
           name)
         (error 'pyffi
                (string-append
                 "Could not find/load libpython.\n"
                 "Set preference 'pyffi:libdir' (raco pyffi configure), or set\n"
                 "environment variable PYFFI_LIBPYTHON to a full path or a\n"
                 "library name (e.g. libpython3.12).")))]))

;; Note: We use #:global? #t so that symbols are visible to extension modules.
;;
;; Errors during path resolution or `ffi-lib` are caught and turned into
;; a #f library handle.  Two reasons:
;;
;;   1. `define-ffi-definer` below caches its lib expression at module
;;      init time, which forces this promise during compilation.  The
;;      Racket package-build server (and any other system that compiles
;;      pyffi without Python installed) needs that step to succeed even
;;      when libpython is absent.  With a #f lib handle, the bindings
;;      defined by `define-python` become "not available" stubs (via
;;      `#:default-make-fail make-not-available`); the module compiles,
;;      and any actual call into Python fails at the call site rather
;;      than at compile time.
;;
;;   2. Downstream consumers that want to require pyffi optionally
;;      (e.g. only when Python is configured) can load the package and
;;      then check `(get-lib)` themselves before calling anything.
(define lib*
  (delay
    (with-handlers ([exn:fail? (λ (_) #f)])
      (ffi-lib (resolve-libpython-path) #:global? #t))))

(define (get-lib)
  (force lib*))

(require pyffi/parameters)

(define-ffi-definer define-python (get-lib)
  #:default-make-fail make-not-available)
