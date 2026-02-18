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
;;   If libpython-folder is a directory, return the path to a file named
;;     libpython3.xx.<extension>
;;   inside that directory, or #f if none is found or folder is #f.
(define (find-libpython3 libpython-folder)
  (cond
    [(not libpython-folder) #f]
    [else
     (define dir (path->complete-path libpython-folder))
     (define rx
       ;; Example on macOS: ^libpython3\.[0-9][0-9]\.dylib$
       (regexp (format "^libpython3\\.[0-9][0-9]~a$"
                       (regexp-quote
                        (string-append "." extension)))))
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
;; IMPORTANT: Delay loading so that compilation/docs don't require libpython.
(define lib*
  (delay (ffi-lib (resolve-libpython-path) #:global? #t)))

(define (get-lib)
  (force lib*))

(require pyffi/parameters)

;; define-python is a macro, but the library expression is evaluated at runtime.
(define-ffi-definer define-python (get-lib)
  #:default-make-fail make-not-available)
