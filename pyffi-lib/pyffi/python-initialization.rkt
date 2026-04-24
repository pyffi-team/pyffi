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

(define home (or (get-preference 'pyffi:home (λ () #f))
                 (get-preference 'pyffi:data (λ () #f))))
(unless home
  (raise (exn:fail:pyffi:not-configured
          (string-join
           '("pyffi is not configured: neither the 'pyffi:home nor 'pyffi:data preference is set."
             "You must set 'pyffi:home (or, for backwards compatibility, 'pyffi:data) to the home"
             "folder of Python.  The most convenient way to do this is to run"
             "`raco pyffi configure`.  See details in the documentation.")
           "\n")
          (current-continuation-marks))))


#;(define program-full-path
   "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10/bin/python3.10")

; (define home "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10")


(define libdir (get-preference 'pyffi:libdir (λ () #f)))
(unless libdir
  (raise (exn:fail:pyffi:not-configured
          (string-join
           '("pyffi is not configured: the 'pyffi:libdir preference is not set."
             "You must set 'pyffi:libdir to the folder containing libpython3."
             "The most convenient way to do this is to run `raco pyffi configure`."
             "See details in the documentation.")
           "\n")
          (current-continuation-marks))))

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
