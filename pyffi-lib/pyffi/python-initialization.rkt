#lang racket/base
(require "libpython.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-delayed.rkt"
         "python-constants.rkt"
         "structs.rkt"
         racket/file
         racket/string)

(provide set-environment-variables
         initialize 
         post-initialize finish-initialization
         diagnostics)

;;;
;;; Configuration
;;;

; (define program-full-path "/Library/Frameworks/Python.framework/Versions/3.10/bin/python3.10")
(define program-full-path "python3.10")

;; PYTHONHOME resolution order:
;;   1. 'pyffi:home preference (explicit user config via raco pyffi configure)
;;   2. 'pyffi:data preference (legacy fallback for old configurations)
;;   3. The natipkg companion's package root, if a natipkg is installed
;;      and bundles a relocatable Python (libpython + stdlib).
;;   4. Error — pyffi is not configured.
(define home
  (or (get-preference 'pyffi:home (λ () #f))
      (get-preference 'pyffi:data (λ () #f))
      (let ([root (pyffi-natipkg-root)])
        (and root (path->string (path->complete-path root))))))
(unless home
  (raise (exn:fail:pyffi:not-configured
          (string-join
           '("pyffi is not configured: neither 'pyffi:home nor 'pyffi:data is set,"
             "and no natipkg companion (e.g. pyffi-aarch64-linux-natipkg) is installed."
             "Either install a natipkg companion to use a bundled Python, or run"
             "`raco pyffi configure /path/to/python3` to point at a system install.")
           "\n")
          (current-continuation-marks))))


;; The actual libpython load is handled by libpython.rkt, which knows
;; how to discover the library across env vars, user prefs, the
;; natipkg companion and the dynamic loader.  python-initialization
;; only needs PYTHONHOME (the `home` value above) for Py_Initialize.

(define (set-environment-variables)
  (define (decode s) (Py_DecodeLocale s #f))
  (Py_SetProgramName (decode "python3.10"))
  ; (Py_SetProgramName (decode (build-path libdir)))
  ; (Py_SetPath (Py_DecodeLocale (get-preference 'pyffi:data (λ () #f)) #f))
  (Py_SetPythonHome  (decode home)))

;;;
;;; Diagnostics
;;;


(define (diagnostics)
  (define (encode s) (and s (Py_EncodeLocale s #f))) ; wchar -> string
  (displayln (list 'ProgramName     (encode (Py_GetProgramName))))
  (displayln (list 'Prefix          (encode (Py_GetPrefix))))
  (displayln (list 'ExecPrefix      (encode (Py_GetExecPrefix))))
  (displayln (list 'ProgramFullPath (encode (Py_GetProgramFullPath))))
  (displayln (list 'Path            (encode (Py_GetPath))))
  (displayln (list 'PythonHome      (encode (Py_GetPythonHome)))))

;;;
;;; Setup Initial Environment
;;;
;;;
;;; Evaluation
;;;


#;(define (initialize)
  (set-environment-variables)
  (Py_Initialize)
  (initialize-main-and-builtins)
  (initialize-builtin-constants) ; uses `run`
  ; We can't run the initialization thunks here.
  ; The Python modules are loaded yet.
  #;(run-initialization-thunks))


(require ffi/unsafe
         #;(only-in ffi/unsafe malloc cast _cpointer ptr-ref cpointer-tag cpointer-push-tag!))


(define (initialize)
  ; (set-environment-variables)
  ; (displayln PyConfig-tag) ; 'PyConfig
  ; (define config (cast (ptr-add (malloc _PyConfig) 0) _pointer _PyConfig-pointer))

  ;; Pre Initialization

  (define preconfig (cast (malloc (ctype-sizeof _PyPreConfig))
                            _pointer _PyPreConfig*))

  ; (define preconfig (make-PyPreConfig 0 0 0 0 0 0 0 0 0 0))
  
  #;(displayln "Before PyPreConfig_InitPythonConfig")
  (PyPreConfig_InitPythonConfig preconfig)
  #;(displayln "PyPreConfig_InitPythonConfig\n")

  
  ; (set-PyPreConfig-utf8_mode! preconfig 1) ; doesn't work on GA

  
  #;(displayln "Before Py_PreInitialize")
  (let ([status (Py_PreInitialize preconfig)])
    (unless (zero? (PyStatus_Exception status))
      (Py_ExitStatusException status)))
  #;(displayln "After Py_PreInitialize\n")

  
  ;; Initialization

  (define config (cast (malloc (ctype-sizeof _PyConfig))
                       _pointer _PyConfig-pointer))
  
  #;(displayln "Before InitPythonConfig")
  (PyConfig_InitPythonConfig config)
  #;(displayln "After InitPythonConfig\n")

  (define (decode s) (Py_DecodeLocale s #f))

  (define pyver      (get-preference 'pyffi:pyver      (λ () "3.12")))
  (define platlibdir (get-preference 'pyffi:platlibdir (λ () "lib")))
  (define venv       (get-preference 'pyffi:venv       (λ () #f)))
  (set-PyConfig-home!       config (decode home))
  (set-PyConfig-platlibdir! config (decode platlibdir))
  
  #;(let ([pythonpath (getenv "PYTHONPATH")])
    (when pythonpath
      (set-PyConfig-pythonpath_env! config (decode pythonpath))))

  ; Leads to error: "invalid memory reference.  Some debugging context lost" on GA
  #;(let ([status (PyConfig_Read config)])
    (unless (zero? (PyStatus_Exception status))
      (Py_ExitStatusException status)))

  
  #;(displayln "Before InitializeFromConfig")
  (let ([status (Py_InitializeFromConfig config)])
    #;(displayln "Before exception check")
    (unless (zero? (PyStatus_Exception status))
      (Py_ExitStatusException status))
    #;(displayln "After InitializeFromConfig"))

  (initialize-main-and-builtins)

  (initialize-builtin-constants) ; uses `run`

  
  ; We can't run the initialization thunks here.
  ; The Python modules are loaded yet.
  #;(run-initialization-thunks))


(define (post-initialize)
  (run-initialization-thunks)
  ; Inject venv site-packages into sys.path when a venv was configured.
  (define venv-pref (get-preference 'pyffi:venv (λ () #f)))
  (define pyver-pref (get-preference 'pyffi:pyver (λ () "3.12")))
  (when venv-pref
    (define sitepkg (string-append venv-pref "/lib/python" pyver-pref "/site-packages"))
    (when (directory-exists? sitepkg)
      (define import-site (PyImport_ImportModule "site"))
      (define sys-mod     (PyImport_ImportModule "sys"))
      (define sys-path    (PyObject_GetAttrString sys-mod "path"))
      (PyList_Append sys-path (PyUnicode_FromString sitepkg))
      (void))))

(define (finish-initialization)
  (run-initialization-thunks))
