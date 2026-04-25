#lang racket/base
(provide (all-defined-out))

;;;
;;; Python C-API
;;;

; This module exports functions from the Python C-API.
;    https://docs.python.org/3/c-api/

; The Python FFI is built on top of the C-API.
; Look in "python.rkt".

;;;
;;; Imports
;;;

(require "libpython.rkt" ffi/unsafe)

(require racket/format racket/match racket/string
         (for-syntax racket/base  racket/format
                     syntax/parse racket/syntax))

;; A handful of Python singletons (Py_False, Py_True, the-None) are
;; computed at module init by calling into libpython.  When libpython
;; isn't loadable (e.g. when the package-build server compiles pyffi
;; without Python installed), `define-python` produces "not available"
;; stubs that error if called.  Wrapping each top-level FFI initialiser
;; in `lazy-ffi` lets module init succeed in that case — the singleton
;; binds to #f, and any actual use will already have failed earlier at
;; `(initialize)`/Py_Initialize_FromConfig time.  See libpython.rkt for
;; the matching tolerance in the lib-load path.
(define-syntax-rule (lazy-ffi expr)
  (with-handlers ([exn:fail? (λ (_) #f)]) expr))
;;;
;;; TYPES
;;;

; The C API uses these C types.
; Most values returned by Python are pointers to a PyObject structure.
; To match the C function signature, we call this type _PyObject*.

; A notable exception are functions returning complex numbers.
; This is done by returning a C structure (i.e. by copying the result values).

(define _FILE*         (_cpointer/null 'FILE))
(define _wchar*        (_cpointer/null 'wchar))
(define _wchar**       (_cpointer/null _wchar*))
(define _size*         (_cpointer/null 'size))

(define _PyModuleDef*  (_cpointer/null 'PyModuleDef))

(define-cstruct _Py_complex ([real _double] [imag _double]))

; Notes:
;  Use Py_DecodeLocale() to decode a bytes string to get a wchar_* string.
;  Use Py_EncodeLocale() the other way.

(define (python-type x)
  (define type (PyObject_Type x))
  (and (PyObject_HasAttrString                   type "__name__")
       (PyUnicode_AsUTF8 (PyObject_GetAttrString type "__name__"))))

;;;
;;; PyObject
;;;

; Notes: Read the explanation of PyObject in Include/object.h
;          https://github.com/python/cpython/blob/main/Include/object.h
; PyObjects
;   - are allocated on a heap
;   - they never move
;   - they begin with two fields: a reference count and a pointer to its type
;   - when the reference count becomes zero, the object is removed
;   - the type is also a PyObject
;   - type of a a type object is a `type`.
;   - the type of `type` is `type`.
;       >>> type(64)
;       <class 'int'>
;       >>> type(type(64))
;       <class 'type'>
;       >>> type(type(type(64)))
;       <class 'type'>

;   - the type pointer of a type points to itself

; To check that an object `ob` has the type `PyBool_Type`,
; we need to check whether `ob->ob_type` equals `&PyBool_Type`.

; The header of a PyObject is:
;    Py_ssize_t ob_refcnt;
;    PyTypeObject *ob_type;

(define-cstruct _PyObject ([ob_refcnt _size]
                           [ob_type   _PyObject-pointer])) ; pointer to _PyObject_Type

(define-cstruct (_PyObjectType _PyObject)
  (; [ob_refcnt _size]
   ; [ob_type   _PyObjectType-pointer]
   [tp_name   _string])) ; does this work?

  
; (define _PyObject*     (_cpointer/null 'PyObject))
; (define _PyObject**    (_cpointer/null _PyObject*))
(define _PyObject*     _PyObject-pointer/null)
(define _PyObject**    (_cpointer/null _PyObject*))

;;;
;;; Will xecutor for Reference Counting
;;;

;; Python uses reference counting.
;; When objects are no longer reachable we decrease the reference
;; count, so the Python VM can remove dead objects.

(define an-executor (make-will-executor))

(void
 (thread
  (λ ()
    (let loop ()
      ; blocks until a new will is ready to execute
      (will-execute an-executor)
      (loop)))))

(define (register-object v)
  (will-register an-executor v executor-proc)
  v)

(define (executor-proc v)
  (printf "a-box is now garbage\n"))

(define (handle-dead-reference v)
  (Py_DecRef v))

(define (new-reference v)
  (will-register an-executor v handle-dead-reference)
  ;(printf "register new reference ")
  ;(print v)
  ;(newline)
  v)

(define (borrowed-reference v)
  (Py_IncRef v)
  (will-register an-executor v handle-dead-reference)
  ;(printf "register new reference ")
  ;(print v)
  ;(newline)
  v)
  

;;;
;;; Initialization, Finalization, and Threads
;;;

; Before we can use the Python interpreter, it needs to be initialized.

;; Initializing and finalizing the interpreter

(define-python Py_Initialize    (_fun      -> _void))
(define-python Py_InitializeEx  (_fun _int -> _void))
(define-python Py_IsInitialized (_fun      -> _int))  ; 0=uninitalized
(define-python Py_FinalizeEx    (_fun      -> _int))
(define-python Py_Finalize      (_fun      -> _void))

;; Process-wide parameters

;; Before Initilization
; These functions should be called before Py_Initialize(), if called at all:
(define-python Py_SetStandardStreamEncoding (_fun _string _string -> _int))
(define-python Py_SetProgramName            (_fun _wchar*         -> _void))
(define-python Py_SetPath                   (_fun _wchar*         -> _void))

;; After Initilization

(define-python Py_GetProgramName            (_fun -> _wchar*)) ; don't call before init.
(define-python Py_GetPrefix                 (_fun -> _wchar*)) ; don't call before init.
(define-python Py_GetExecPrefix             (_fun -> _wchar*)) ; don't call before init.
(define-python Py_GetProgramFullPath        (_fun -> _wchar*)) ; don't call before init.
(define-python Py_GetPath                   (_fun -> _wchar*))
(define-python Py_GetPythonHome             (_fun -> _wchar*)) ; don't call before init.
; (Py_EncodeLocale (Py_GetPath) #f)

;; Before or After Initilization
(define-python Py_GetVersion                (_fun -> _string))
(define-python Py_GetPlatform               (_fun -> _string))
(define-python Py_GetCopyright              (_fun -> _string))
(define-python Py_GetCompiler               (_fun -> _string))
(define-python Py_GetBuildInfo              (_fun -> _string))
(define-python PySys_SetArgvEx              (_fun _int _wchar** _int -> _void))
(define-python PySys_SetArgv                (_fun _int _wchar**      -> _void))
(define-python Py_SetPythonHome             (_fun _wchar*            -> _void))


;;;
;;; Reference Counting
;;;

(define-python Py_NewRef  (_fun _PyObject* -> _PyObject*)) ; o must not be NULL
(define-python Py_XNewRef (_fun _PyObject* -> _PyObject*)) ; returns NULL given NULL
(define-python Py_IncRef  (_fun _PyObject* -> _void))      ; increment reference count
(define-python Py_DecRef  (_fun _PyObject* -> _void))      ; decrement reference count



;;;
;;; Boolean Objects
;;;

;; Booleans in Python are implemented as a subclass of integers.
;; There are only two booleans: Py_False and Py_True.
;; As such, the normal creation and deletion functions don’t apply to booleans.

;; Note: It's easier to use PyBool_FromLong than return PyFalse and Py_True
;;       directly. If PyFalse are Py_True returned from a function,
;;       their reference count needs to be incremented.

(define-python PyBool_FromLong (_fun _long -> [o : _PyObject*] -> (new-reference o)))
; Return a new reference to Py_True or Py_False depending on the truth value of v.

(define Py_False (lazy-ffi (PyBool_FromLong 0)))
;   The Python False object. This object has no methods.
;   It needs to be treated just like any other object with respect to reference counts.
;   When used as a return value, remember to increment the reference count.
;   [Use Py_RETURN_FALSE]

(define Py_True (lazy-ffi (PyBool_FromLong 1)))
; The Python True object. This object has no methods.
; It needs to be treated just like any other object with respect to reference counts.
;   When used as a return value, remember to increment the reference count.
;   [Use Py_RETURN_TRUE]

(define-python PyBool_Type _PyObjectType)

(define (PyBool_Check ob*) ; C Macro in the API
  (equal? (PyObject-ob_type ob*) PyBool_Type))

(define (Py_RETURN_FALSE) (Py_NewRef Py_False))
(define (Py_RETURN_TRUE)  (Py_NewRef Py_True))


;;;
;;; Exception Handling
;;;

;; Quote:
;;   > The functions described in this chapter will let you handle and raise
;;   > Python exceptions. It is important to understand some of the basics of
;;   > Python exception handling. It works somewhat like the POSIX errno variable:
;;   > there is a global indicator (per thread) of the last error that occurred.
;;   > Most C API functions don’t clear this on success, but will set it to indicate
;;   > the cause of the error on failure. Most C API functions also return an error
;;   > indicator, usually NULL if they are supposed to return a pointer, or -1 if
;;   > they return an integer (exception: the PyArg_* functions return 1 for
;;   > success and 0 for failure).

; Printing and Clearing
(define-python PyErr_Clear    (_fun      -> _void)) ; clear error indicator
(define-python PyErr_PrintEx  (_fun _int -> _void)) ; print error and clear
(define-python PyErr_Print    (_fun      -> _void)) ; alias
(define-python PyErr_WriteUnraisable (_fun _PyObject* -> _void))
; Raising Exceptions
(define-python PyErr_SetString (_fun _PyObject* _string -> _void)) ; set error indicator
; ...
; Querying the error indicator
(define-python PyErr_Occurred (_fun -> [o : _PyObject*] -> (borrowed-reference o))) ; caller must hold GIL, borrowed reference


(define-python PyException_GetTraceback (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyException_GetContext   (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyException_GetCause     (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))

; void PyErr_Fetch(PyObject **ptype, PyObject **pvalue, PyObject **ptraceback)¶
(define-python PyErr_Fetch     (_fun (ptype      : (_ptr o _PyObject*)) ; todo: ref count
                                     (pvalue     : (_ptr o _PyObject*))
                                     (ptraceback : (_ptr o _PyObject*))
                                     -> _void
                                     -> (values ptype pvalue ptraceback)))
; void PyErr_NormalizeException(PyObject **exc, PyObject **val, PyObject **tb)
(define-python PyErr_NormalizeException
  (_fun (ptype      : (_ptr io _PyObject*))
        (pvalue     : (_ptr io _PyObject*))
        (ptraceback : (_ptr io _PyObject*))
        -> _void
        -> (values ptype pvalue ptraceback)))

;;;
;;; NUMBERS
;;;

; Python itself has 3 number types:
;   integers, floats and complex
; However Numpy has many, more preceise number types.
;   https://numpy.org/doc/stable/reference/arrays.scalars.html

(define-python PyNumber_Add            (_fun _PyObject*  _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyNumber_Subtract       (_fun _PyObject*  _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyNumber_Multiply       (_fun _PyObject*  _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyNumber_MatrixMultiply (_fun _PyObject*  _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyNumber_FloorDivide    (_fun _PyObject*  _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyNumber_TrueDivide     (_fun _PyObject*  _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyNumber_Remainder      (_fun _PyObject*  _PyObject* -> [o : _PyObject*] -> (new-reference o)))
;; (define-python PyNumber_Divmod         (_fun _PyObject*  _PyObject* -> _PyObject*))
;; (define-python PyNumber_Power          (_fun _PyObject*  _PyObject* -> _PyObject*))
;; (define-python PyNumber_Multiply       (_fun _PyObject*  _PyObject* -> _PyObject*))




;;;
;;; Complex Numbers
;;;

; Quote:
;  > Python’s complex number objects are implemented as two distinct types
;  > when viewed from the C API: one is the Python object exposed to Python
;  > programs, and the other is a C structure which represents the actual
;  > complex number value.

; When views from C we will use _PyComplex to represent a C structure
; holding the real and imaginary parts as floats.

(define-python PyComplex_AsCComplex   (_fun _PyObject*  -> _Py_complex))                               ; Py -> C
(define-python PyComplex_FromCComplex (_fun _Py_complex -> [o : _PyObject*] -> (new-reference o)))     ; C  -> Py
(define-python PyComplex_FromDoubles  (_fun _double _double -> [o : _PyObject*] -> (new-reference o))) ; make-rectangular
(define-python PyComplex_RealAsDouble (_fun _PyObject* -> _double))                                    ; get real part
(define-python PyComplex_ImagAsDouble (_fun _PyObject* -> _double))                                    ; get imaginary part
  

;;;
;;; Floating Point Objects
;;;

(define-python PyFloat_FromString  (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o))) ; 
(define-python PyFloat_FromDouble  (_fun _double    -> [o : _PyObject*] -> (new-reference o)))
(define-python PyFloat_AsDouble    (_fun _PyObject* -> _double))
(define-python PyFloat_GetInfo     (_fun            -> [o : _PyObject*] -> (new-reference o))) ; structseq instance
(define-python PyFloat_GetMax      (_fun            -> _double))
(define-python PyFloat_GetMin      (_fun            -> _double))


;;;
;;; Integer Objects
;;;

; Quote:
;   > All integers are implemented as “long” integer objects of arbitrary size.
;   > On error, most PyLong_As* APIs return (return type) -1 which cannot be
;   > distinguished from a number.
;   > Use PyErr_Occurred() to disambiguate.

; That is: A PyLong corresponds to a Racket integer.
; If -1 is returned, an error check is needed.
; At least from some of the functions.

; (define-python PyLong_CheckExact (_fun _PyObject* -> ))
; Note: This was documented as a function, but was in fact a C macro.

(define-python PyLong_FromLong (_fun _long -> [o : _PyObject*] -> (new-reference o)))
; Return a new PyLongObject object from v, or NULL on failure.
;    The current implementation keeps an array of integer objects for all
;    integers between -5 and 256. When you create an int in that range you
;    actually just get back a reference to the existing object.

(define-python PyLong_FromUnsignedLong     (_fun _ulong  -> [o : _PyObject*] -> (new-reference o)))
(define-python PyLong_FromLongLong         (_fun _llong  -> [o : _PyObject*] -> (new-reference o)))
(define-python PyLong_FromUnsignedLongLong (_fun _ullong -> [o : _PyObject*] -> (new-reference o)))
(define-python PyLong_FromDouble           (_fun _double -> [o : _PyObject*] -> (new-reference o)))

; PyObject *PyLong_FromString(const char *str, char **pend, int base)
(define-python PyLong_FromString        (_fun _string _pointer _int -> [o : _PyObject*] -> (new-reference o)))
(define-python PyLong_FromUnicodeObject (_fun _PyObject* _int       -> [o : _PyObject*] -> (new-reference o)))
; PyObject *PyLong_FromVoidPtr(void *p)
(define-python PyLong_FromVoidPtr (_fun _pointer -> [o : _PyObject*] -> (new-reference o)))
(define-python PyLong_AsLong (_fun _PyObject* -> _long)) ; -1 is error (i.e. out of range)
; long PyLong_AsLongAndOverflow(PyObject *obj, int *overflow)
(define-python PyLong_AsLongAndOverflow (_fun (long_onj : _PyObject*)  ; todo ref counting
                                              (out : (_ptr o _int))
                                              -> (long : _long)
                                              -> (values long out)))
(define-python PyLong_AsLongLong (_fun _PyObject* -> _llong))
(define-python PyLong_AsLongLongAndOverflow (_fun (llong_onj : _PyObject*)
                                                  (out : (_ptr o _int))
                                              -> (llong : _llong)
                                              -> (values llong out)))

(define-python PyLong_AsUnsignedLong         (_fun _PyObject* -> _ulong))
(define-python PyLong_AsUnsignedLongLong     (_fun _PyObject* -> _ullong))
(define-python PyLong_AsUnsignedLongMask     (_fun _PyObject* -> _ulong))
(define-python PyLong_AsUnsignedLongLongMask (_fun _PyObject* -> _ullong))
(define-python PyLong_AsDouble               (_fun _PyObject* -> _double))
(define-python PyLong_AsVoidPtr              (_fun _PyObject* -> _pointer))

; (define-python PyLong_Check (_fun _PyObject* -> _int))

; Note: This was documented as a function, but was in fact a C macro.
;   Return true if its argument is a PyLongObject or a subtype of PyLongObject.
;   This function (sic) always succeeds.


;;;
;;; Unicode (Python Strings)
;;;

(define-python PyUnicode_AsUTF8     (_fun _PyObject* -> _string))
(define-python PyUnicode_FromString (_fun _string    -> [o : _PyObject*] -> (new-reference o))) ; utf8

(define-python PyUnicode_Substring  (_fun _PyObject* _size _size -> [o : _PyObject*] -> (new-reference o)))

;;;
;;; Building Value
;;;

(define-python Py_BuildValue0 (_fun #:varargs-after 1 _string -> [o : _PyObject*] -> (new-reference o))
  #:c-id Py_BuildValue)

(define the-None (lazy-ffi (Py_BuildValue0 "")))
(define (build-None)
  (Py_IncRef the-None)
  the-None)

;;;
;;; List Objects
;;;

(define-python PyList_New     (_fun _size -> [o : _PyObject*] -> (new-reference o)))
(define-python PyList_Size    (_fun _PyObject* -> _size))
(define-python PyList_GetItem (_fun _PyObject* _size -> [o : _PyObject*] -> (borrowed-reference o))) ; borrowed
(define-python PyList_SetItem (_fun _PyObject* _size _PyObject* -> _int)) ; 0 on success   ; steals reference 
(define-python PyList_Insert  (_fun _PyObject* _size _PyObject* -> _int)) ; 0 on success
; Analogous to list.insert(index, item).
(define-python PyList_Append  (_fun _PyObject* _PyObject* -> _int))       ; 0 on success
; Append item. Analogous to list.append(item).

(define-python PyList_GetSlice (_fun _PyObject* _size _size -> [o : _PyObject*] -> (new-reference o)))
(define-python PyList_SetSlice (_fun _PyObject* _size _size _PyObject* -> _int))
(define-python PyList_Sort     (_fun _PyObject* -> _int))
(define-python PyList_Reverse  (_fun _PyObject* -> _int))
(define-python PyList_AsTuple  (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))

;;;
;;; Slice Objects
;;;

(define-python PySlice_New        (_fun _PyObject* _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PySlice_GetIndices (_fun _PyObject* _size
                                        (start      : (_ptr o _size))
                                        (stop       : (_ptr o _size))
                                        (end        : (_ptr o _size))
                                        -> _void
                                        -> (values start stop end)))

;;;
;;; Iterator Protocol
;;;

(define-python PyIter_Check       (_fun _PyObject* -> _int))



;;;
;;; Tuple Objects
;;;

(define-python PyTuple_New (_fun _size -> [o : _PyObject*] -> (new-reference o)))
; PyObject *PyTuple_Pack(Py_ssize_t n, ...)

; TODO: #:varargs-after available in Racket 7.9 and later
;; (define-python PyTuple_Pack (_fun #:varargs-after 1
;;                                   _size _PyObject* -> _PyObject*))

(define-python PyTuple_Size     (_fun _PyObject*             -> _size))
(define-python PyTuple_GetItem  (_fun _PyObject* _size       -> [o : _PyObject*] -> (borrowed-reference o))) ; borrowed
(define-python PyTuple_GetSlice (_fun _PyObject* _size _size -> [o : _PyObject*] -> (new-reference o))) 
(define-python PyTuple_SetItem  (_fun _PyObject* _size _PyObject* -> _int)) ; 0 on success

;;;
;;; Dictionary Objects
;;;

(define-python PyDict_New              (_fun                                  -> [o : _PyObject*] -> (new-reference o)))
(define-python PyDictProxy_New         (_fun _PyObject*                       -> [o : _PyObject*] -> (new-reference o)))
(define-python PyDict_Clear            (_fun _PyObject*                       -> _void))
(define-python PyDict_Contains         (_fun _PyObject* _PyObject*            -> _int))
(define-python PyDict_Copy             (_fun _PyObject*                       -> [o : _PyObject*] -> (new-reference o)))
(define-python PyDict_SetItem          (_fun _PyObject* _PyObject* _PyObject* -> _int)) ; 0 on success, does NOT steal reference
(define-python PyDict_SetItemString    (_fun _PyObject* _string    _PyObject* -> _int)) ; 0 on success
(define-python PyDict_DelItem          (_fun _PyObject* _PyObject*            -> _int)) ; 0 on success
(define-python PyDict_DelItemString    (_fun _PyObject* _string               -> _int)) ; 0 on success
(define-python PyDict_GetItem          (_fun _PyObject* _PyObject*            -> [o : _PyObject*] -> (borrowed-reference o))) ; borrowed
(define-python PyDict_GetItemWithError (_fun _PyObject* _PyObject*            -> [o : _PyObject*] -> (borrowed-reference o))) ; borrowed
(define-python PyDict_GetItemString    (_fun _PyObject* _string               -> [o : _PyObject*] -> (borrowed-reference o))) ; borrowed
(define-python PyDict_SetDefault       (_fun _PyObject* _PyObject* _PyObject* -> [o : _PyObject*] -> (borrowed-reference o))) ; borrowed
(define-python PyDict_Items            (_fun _PyObject*                       -> [o : _PyObject*] -> (new-reference o))) ; PyListObject
(define-python PyDict_Keys             (_fun _PyObject*                       -> [o : _PyObject*] -> (new-reference o))) ; PyListObject
(define-python PyDict_Values           (_fun _PyObject*                       -> [o : _PyObject*] -> (new-reference o))) ; PyListObject
(define-python PyDict_Size             (_fun _PyObject*                       -> _size))
(define-python PyDict_Next             (_fun _PyObject* _pointer (key : (_ptr io _PyObject*)) (val : (_ptr io _PyObject*)) 
                                             -> (flag : _int) -> (list flag (borrowed-reference key) (borrowed-reference val))))
(define-python PyDict_Merge            (_fun _PyObject* _PyObject* _int       -> _int))
(define-python PyDict_Update           (_fun _PyObject* _PyObject*            -> _int))
(define-python PyDict_MergeFromSeq2    (_fun _PyObject* _PyObject* _int       -> _int))



;;;
;;; Mappping Protocol
;;;

(define-python PyMapping_Check         (_fun _PyObject*                       -> _int))


;;;
;;; Module Objects
;;;

(define-python PyModule_NewObject         (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))    ; name -> module
(define-python PyModule_New               (_fun _string    -> [o : _PyObject*] -> (new-reference o)))       ; name -> module

(define-python PyModule_GetDict           (_fun _PyObject* -> [o : _PyObject*] -> (borrowed-reference o)))    ; module -> dict           borrowed
(define-python PyModule_GetNameObject     (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))    ; module -> __name__
(define-python PyModule_GetName           (_fun _PyObject* -> _string))       ; module -> name
(define-python PyModule_GetState          (_fun _PyObject* -> _pointer))      ; module -> ...
(define-python PyModule_GetDef            (_fun _PyObject* -> _PyModuleDef*)) ; module -> module-definition
(define-python PyModule_GetFilenameObject (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))    ; module -> filename
(define-python PyModule_GetFilename       (_fun _PyObject* -> _string))       ; module -> filename as string

(define-python PyImport_ImportModule      (_fun _string -> [o : _PyObject*] -> (new-reference o)))
; (define-python PyImport_ImportModuleEx    (_fun _string _PyObject* _PyObject* _PyObject* -> _PyObject*))
(define-python PyImport_ImportModuleLevel  (_fun _string _PyObject* _PyObject* _PyObject* _int -> [o : _PyObject*] -> (new-reference o)))
(define (PyImport_ImportModuleEx n g l f) (PyImport_ImportModuleLevel n g l f 0))


(define-python PyModule_AddObjectRef      (_fun _PyObject* _string _PyObject* -> _int))


;;;
;;; Operating System Utilities
;;;

(define-python PyOS_FSPath                (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))  ; str or bytes -> ...
(define-python Py_FdIsInteractive         (_fun _FILE* _string -> _int))
(define-python PySys_GetObject            (_fun _string -> [o : _PyObject*] -> (borrowed-reference o)))  ; borrowed

(define-python Py_EncodeLocale            (_fun _wchar* _size* -> _string))
(define-python Py_DecodeLocale            (_fun _string _size* -> _wchar*))

; (Py_EncodeLocale (cast (PySys_GetObject "path") _PyObject* _wchar*) #f)

;;;
;;; Importing Modules
;;;

(define-python PyImport_Import    (_fun _string -> [o : _PyObject*] -> (new-reference o)))
(define-python PyImport_AddModule (_fun _string -> [o : _PyObject*] -> (borrowed-reference o))) ; borrowed

;(define-python PyModule_GetName   (_fun _PyObject* -> _string))

;;;
;;; Common Object Structures
;;;

(define-python Py_Is      (_fun _PyObject* _PyObject* -> _int)) ; x is y?
(define-python Py_IsNone  (_fun _PyObject* -> _int))            ; is x the None  singleton?
(define-python Py_IsTrue  (_fun _PyObject* -> _int))            ; is x the True  singleton?
(define-python Py_IsFalse (_fun _PyObject* -> _int))            ; is x the False singleton?
; (define-python Py_TYPE    (_fun _PyObject* -> _PyTypeObject*)) ; TODO: a C macro

(define-python PyCallable_Check (_fun _PyObject* -> _int)) ; 1 means callable, 0 otherwise

(define-python PyObject_Call       (_fun _PyObject* _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o)))
(define-python PyObject_CallNoArgs (_fun _PyObject*                       -> [o : _PyObject*] -> (new-reference o)))
; (define-python PyObject_CallOneArg (_fun _PyObject* _PyObject*            -> _PyObject*))
; PyObject_CallOneArg was a static inline function, and not exported in the shared library

; PyObject *PyObject_CallMethodObjArgs(PyObject *obj, PyObject *name, ...)
(define-python PyObject_CallMethodObjArgs0
  (_fun #:varargs-after 2 _PyObject*  _PyObject* -> [o : _PyObject*] -> (new-reference o))
  #:c-id PyObject_CallMethodObjArgs)
(define-python PyObject_CallMethodObjArgs1
  (_fun #:varargs-after 2 _PyObject*  _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o))
  #:c-id PyObject_CallMethodObjArgs)
(define-python PyObject_CallMethodObjArgs2
  (_fun #:varargs-after 2 _PyObject*  _PyObject* _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o))
  #:c-id PyObject_CallMethodObjArgs)
(define-python PyObject_CallMethodObjArgs3
  (_fun #:varargs-after 2 _PyObject*  _PyObject* _PyObject* _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o))
  #:c-id PyObject_CallMethodObjArgs)
(define-python PyObject_CallMethodObjArgs4
  (_fun #:varargs-after 2 _PyObject*  _PyObject* _PyObject* _PyObject* _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o))
  #:c-id PyObject_CallMethodObjArgs)
(define-python PyObject_CallMethodObjArgs5
  (_fun #:varargs-after 2 _PyObject*  _PyObject* _PyObject* _PyObject* _PyObject* _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o))
  #:c-id PyObject_CallMethodObjArgs)
(provide PyObject_CallMethodObjArgs)
(define PyObject_CallMethodObjArgs
  (case-lambda
    [(obj name)                (PyObject_CallMethodObjArgs0 obj name)]
    [(obj name a1)             (PyObject_CallMethodObjArgs1 obj name a1)]
    [(obj name a1 a2)          (PyObject_CallMethodObjArgs2 obj name a1 a2)]
    [(obj name a1 a2 a3)       (PyObject_CallMethodObjArgs3 obj name a1 a2 a3)]
    [(obj name a1 a2 a3 a4)    (PyObject_CallMethodObjArgs4 obj name a1 a2 a3 a4)]
    [(obj name a1 a2 a3 a4 a5) (PyObject_CallMethodObjArgs5 obj name a1 a2 a3 a4 a5)]
    [else (error 'PyObject_CallMethodObjArgs "At most 5 arguments are supported for a method call.")]))

; (PyObject_Call callable (PyTuple_New 0) #f)


;;;
;;; Object Protocol
;;;

(define-python PyObject_Repr (_fun _PyObject* -> [o : _PyObject*] -> (new-reference o)))

(define-python PyEval_GetGlobals (_fun -> [o : _PyObject*] -> (borrowed-reference o))) ; -> dictionary

(define-python PyObject_GetAttrString (_fun _PyObject* _string            -> [o : _PyObject*] -> (new-reference o)))
(define-python PyObject_HasAttrString (_fun _PyObject* _string            -> _int))
(define-python PyObject_Type          (_fun _PyObject*                    -> [o : _PyObject*] -> (new-reference o)))
(define-python PyObject_IsTrue        (_fun _PyObject*                    -> _int))
(define-python PyObject_Str           (_fun _PyObject*                    -> [o : _PyObject*] -> (new-reference o)))
(define-python PyObject_SetAttrString (_fun _PyObject* _string _PyObject* -> _int))

(define-python PyObject_GetItem       (_fun _PyObject* _PyObject*         -> [o : _PyObject*] -> (new-reference o)))

(define-python PyObject_Length        (_fun _PyObject*                    -> _int))



;;;
;;; Bytes
;;;

;; Bytes objects are immutable sequences of single bytes. 

(define-python PyBytes_FromString        (_fun _string       -> [o : _PyObject*] -> (new-reference o)))
(define-python PyBytes_FromStringAndSize (_fun _string _size -> [o : _PyObject*] -> (new-reference o)))

(define-python PyBytes_AsString          (_fun _PyObject*    -> _bytes))

; char *PyBytes_AsString(PyObject *o)
; (define-python PyBytes_AsString          (_fun _PyObject* -> _bytes)) ; xxx


;;;
;;; The Very High Level Layer
;;;

; int PyRun_SimpleString(const char *command)
(define-python PyRun_SimpleString (_fun _string -> _int))

(define-python PyRun_String (_fun _string _int _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o)))

;; PyObject *PyRun_String(const char *str, int start, PyObject *globals, PyObject *locals)
;; Return value: New reference.


; PyObject *Py_CompileString(const char *str, const char *filename, int start)
(define-python Py_CompileString (_fun _string _string _int -> [o : _PyObject*] -> (new-reference o)))

; PyObject *PyEval_EvalCode(PyObject *co, PyObject *globals, PyObject *locals)
(define-python PyEval_EvalCode (_fun _PyObject* _PyObject* _PyObject* -> [o : _PyObject*] -> (new-reference o))) 

;;;
;;; PyWideStringList
;;;

;; typedef struct {
;;     /* If length is greater than zero, items must be non-NULL
;;        and all items strings must be non-NULL */
;;     Py_ssize_t length;
;;     wchar_t **items;
;;     } PyWideStringList;

(define-cstruct _PyWideStringList
  ([length _ssize]   ;   /* _PyConfigInitEnum value */
   [items  _wchar**]))

(define _PyWideStringList* _PyWideStringList-pointer/null)

;;;
;;; PyStatus
;;;

(define _char* (_cpointer/null _int8))  ; assume _char is a byte

(define-cstruct _PyStatus
  ([_type (_enum '(_PyStatus_TYPE_OK    = 0
                   _PyStatus_TYPE_ERROR = 1
                   _PyStatus_TYPE_EXIT  = 2))]
   [func     _char*]
   [err_msg  _char*]
   [exitcode _int]))

(define-python PyStatus_Exception     (_fun _PyStatus -> _int))
(define-python Py_ExitStatusException (_fun _PyStatus -> _void))

;;;
;;; PyPreConfig
;;;

(define-cstruct _PyPreConfig
  ([_config_init _int] 
   [parse_argv _int]
   [isolated _int]
   [use_environment _int]
   [configure_locale _int]
   [coerce_c_locale _int]
   [coerce_c_locale_warn _int]
   ;; #ifdef MS_WINDOWS
   ;;     /* If greater than 1, use the "mbcs" encoding instead of the UTF-8
   ;;        encoding for the filesystem encoding.
   ;;        Set to 1 if the PYTHONLEGACYWINDOWSFSENCODING environment variable is
   ;;        set to a non-empty string. If set to -1 (default), inherit
   ;;        Py_LegacyWindowsFSEncodingFlag value.
   ;;        See PEP 529 for more details. */
   ;;     int legacy_windows_fs_encoding;
   ;; #endif
   [utf8_mode _int]
   [dev_mode _int]
   [allocator _int]))
(define _PyPreConfig* _PyPreConfig-pointer/null)

(define-python PyPreConfig_InitPythonConfig   (_fun _PyPreConfig* -> _void))
(define-python PyPreConfig_InitIsolatedConfig (_fun _PyPreConfig* -> _void))

(define-python Py_PreInitialize (_fun _PyPreConfig* -> _PyStatus))


;;;
;;; PyConfig
;;;

(define-cstruct _PyConfig
  ([_config_init _int]   ;   /* _PyConfigInitEnum value */

   [isolated                _int]
   [use_environment         _int]
   [dev_mode                _int]
   [install_signal_handlers _int]
   [use_hash_seed           _int]
   [hash_seed               _ulong]
   [faulthandler            _int]
   [tracemalloc             _int]
   [perf_profiling          _int]
   [import_time             _int]
   [code_debug_ranges       _int]
   [show_ref_count          _int]
   [dump_refs               _int]
   [dump_refs_file          _wchar*]
   [malloc_stats            _int]
   [filesystem_encoding     _wchar*]
   [filesystem_errors       _wchar*]
   [pycache_prefix          _wchar*]
   [parse_argv              _int]
   [orig_argv               _PyWideStringList]
   [argv                    _PyWideStringList]
   [xoptions                _PyWideStringList]
   [warnoptions             _PyWideStringList]
   [site_import _int]
   [bytes_warning _int]
   [warn_default_encoding _int]
   [inspect _int]
   [interactive _int]
   [optimization_level _int]
   [parser_debug _int]
   [write_bytecode _int]
   [verbose _int]
   [quiet _int]
   [user_site_directory _int]
   [configure_c_stdio _int]
   [buffered_stdio _int]
   [stdio_encoding _wchar*]
   [stdio_errors _wchar*]   
   ;; #ifdef MS_WINDOWS
   ;;     int legacy_windows_stdio;
   ;; #endif
   [check_hash_pycs_mode _wchar*]
   [use_frozen_modules _int]
   [safe_path _int]
   ;; /* --- Path configuration inputs ------------ */
   [pathconfig_warnings _int]
   [program_name _wchar*]
   [pythonpath_env _wchar*]
   [home _wchar*]
   [platlibdir _wchar*]

   ;; /* --- Path configuration outputs ----------- */
   [module_search_paths_set _int]
   [module_search_paths _PyWideStringList]
   [stdlib_dir _wchar*]
   [executable _wchar*]
   [base_executable _wchar*]
   [prefix _wchar*]
   [base_prefix _wchar*]
   [exec_prefix _wchar*]
   [base_exec_prefix _wchar*]

   ;; /* --- Parameter only used by Py_Main() ---------- */
   [skip_source_first_line _int]
   [run_command  _wchar*]
   [run_module   _wchar*]
   [run_filename _wchar*]
   ;; /* --- Private fields ---------------------------- */
   ;; // Install importlib? If equals to 0, importlib is not initialized at all.
   ;; // Needed by freeze_importlib.
   [_install_importlib _int];
   ;; // If equal to 0, stop Python initialization before the "main" phase.
   [_init_main _int ]
   ;; // If non-zero, disallow threads, subprocesses, and fork.
   ;; // Default: 0.
   [_isolated_interpreter _int]
   ;; // If non-zero, we believe we're running from a source tree.
   [_is_python_build _int]))
(define _PyConfig* _PyConfig-pointer)



;; typedef struct {
;;     enum {
;;         _PyStatus_TYPE_OK=0,
;;         _PyStatus_TYPE_ERROR=1,
;;         _PyStatus_TYPE_EXIT=2
;;     } _type;
;;     const char *func;
;;     const char *err_msg;
;;     int exitcode;
;;     } PyStatus;




(define-python PyConfig_InitPythonConfig   (_fun _PyConfig* -> _void))
(define-python PyConfig_InitIsolatedConfig (_fun _PyConfig* -> _void))

(define-python PyConfig_SetString          (_fun _PyConfig* _wchar* _wchar*  -> _PyStatus))
(define-python PyConfig_SetBytesString     (_fun _PyConfig* _wchar*  _char*  -> _PyStatus))

(define-python PyConfig_SetArgv            (_fun _PyConfig* _int _wchar*  -> _PyStatus))
(define-python PyConfig_SetBytesArgv       (_fun _PyConfig* _int  _char*  -> _PyStatus))


(define-python PyConfig_SetWideStringList  (_fun _PyConfig* _int _PyWideStringList*  _ssize _wchar** -> _PyStatus))

(define-python PyConfig_Read               (_fun _PyConfig*  -> _PyStatus))
(define-python PyConfig_Clear              (_fun _PyConfig*  -> _void))

(define-python Py_InitializeFromConfig     (_fun _PyConfig* -> _PyStatus))






