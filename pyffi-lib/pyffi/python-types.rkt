#lang racket/base
(provide (all-defined-out)
         pr 
         ->)

(require "structs.rkt"
         "python-c-api.rkt"
         "python-evaluation.rkt"
         "python-environment.rkt"
         "python-initialization.rkt"
         "python-delayed.rkt"
         racket/format
         racket/list
         racket/match
         racket/string)

(require (for-syntax racket/base
                     racket/format
                     racket/match
                     racket/syntax                     
                     syntax/parse))

(require (only-in ffi/unsafe -> cpointer?))
(require (for-syntax (only-in ffi/unsafe ->)))

;;;
;;; PYTHON -> RACKET
;;;

(define (py-int->number x)
  (define l (PyLong_AsLong x))
  (if (= l -1) ; indicates an error
      (string->number (PyUnicode_AsUTF8 (PyObject_Str x)))
      l))

(define (py-float->number x)
  (PyFloat_AsDouble x))

(define (py-complex->number x)
  (define c (PyComplex_AsCComplex x))
  (make-rectangular (Py_complex-real c) (Py_complex-imag c)))

;;;
;;; RACKET -> PYTHON
;;;

(define (integer->py-int x)
  (unless (integer? x) (error 'integer->py-int  "got: ~a" x))
  (run (number->string x)))  ; todo!

(define (inexact->py-float x)
  (unless (real? x) (error 'inexact->py-float "got: ~a" x))
  (case x
    [(+nan.0)    (run "float('nan')")]
    [else        (if (inexact? x)
                     (run (number->string x))             ; todo
                     (run (number->string (* 1.0 x))))])) ; todo

(define (string->py-string x)
  (unless (string? x) (error 'string->py-string "got: ~a" x))
  (PyUnicode_FromString x))

(define (py-string->string x)
  (PyUnicode_AsUTF8 x))


(define (vector->py-tuple x)
  (unless (vector? x) (error 'vector->py-tuple "got: ~a" x))
  (define n (vector-length x))
  (define t (PyTuple_New n))
  (for ([i (in-range n)])
    (define v (racket->python (vector-ref x i)))
    (Py_IncRef v) ; since Tuple_SetItem steals reference
    (PyTuple_SetItem t i v))
  t)

(define (flat-vector->py-tuple x)
  ; use vector elements as-is.
  (unless (vector? x) (error 'flat-vector->py-tuple "got: ~a" x))
  (define n (vector-length x))
  (define t (PyTuple_New n))
  (for ([i (in-range n)])
    (define v (vector-ref x i))
    (Py_IncRef v) ; since Tuple_SetItem steals reference
    (PyTuple_SetItem t i v))
  t)

(define (py-tuple->vector x)
  (define n (PyTuple_Size x))
  (define v (make-vector n))
  (for ([i (in-range n)])
    (vector-set! v i (python->racket (PyTuple_GetItem x i))))
  v)

(define (py-list->list x)
  (define n (PyList_Size x))
  (for/list ([i (in-range n)])
    (python->racket (PyList_GetItem x i))))

(define (py-list->list/pr x)
  (define n (PyList_Size x))
  (for/list ([i (in-range n)])
    (pr (PyList_GetItem x i))))

(define (list->py-list xs)
  (unless (list? xs) (error 'list->py-list "got: ~a" xs))
  (define n (length xs))
  (define ys (PyList_New n))
  (for ([i (in-range n)] [x (in-list xs)])
    (define v (racket->python x))
    (Py_IncRef v) ; since PyList_SetItem steals reference
    (PyList_SetItem ys i v))
  ys)


(define (racket->python x)
  (cond
    [(number? x) (cond
                   [(integer? x) (integer->py-int x)]
                   [(inexact? x) (inexact->py-float x)]
                   ; complex
                   [else (error 'racket->python "got: ~a" x)])]
    [(obj? x)     (obj-the-obj x)]
    [(boolean? x) (boolean->py-bool x)]
    [(string? x)  (string->py-string x)]
    [(vector? x)  (vector->py-tuple x)]
    [(list? x)    (list->py-list x)]
    [(void? x)    (build-None)]
    [else         (error 'racket->python "got: ~a" x)]))

(define (boolean->py-bool x)
  ; Note: this creates a new reference
  (PyBool_FromLong (if x 1 0)))

(define (py-bool->boolean x)
  (if (= (PyObject_IsTrue x) 1) #t #f))


(define (rp x)
  (cond
    [(number? x) (cond
                   [(integer? x) (integer->py-int x)]
                   [(inexact? x) (inexact->py-float x)]
                   ; complex
                   [else (error 'rp "got: ~a" x)])]
    [(obj? x)     (obj-the-obj x)]
    [(boolean? x) (boolean->py-bool x)]
    [(string? x)  (string->py-string x)]
    [(vector? x)  (vector->py-tuple x)]
    [(list? x)    (list->py-list x)]
    [(void? x)    (build-None)]
    [else         (error 'rp "got: ~a" x)]))


;; An easy way to see the type strings in the Python repl.
;;   >>> getattr(type(5.0),"__name__")
;;   'float'

(define (pr/false x)
  (if (eq? x #f) x (pr x)))


(define py-format-exception      'uninitialized-py-format-exception)
(define py-format-exception-only 'uninitialized-py-format-exception-only)
(define py-format-traceback      'uninitialized-py-format-traceback)

(add-initialization-thunk
 (λ ()
   (set! py-format-exception      (get 'traceback.format_exception))
   (set! py-format-exception-only (get 'traceback.format_exception_only))
   (set! py-format-traceback      (get 'traceback.format_tb))))

;; Canonical handler for Python exceptions surfaced through pyffi.
;;
;; `result` is the C-API call's return value: a Python object on
;; success, `#f` on failure (Python convention is NULL on failure;
;; pyffi's FFI binding maps NULL to #f).  When `result` is truthy the
;; macro yields it directly, or `(wrap-fn result)` when a `#:wrap`
;; clause is supplied (used by `python-functions.rkt` to apply a
;; per-call result-converter).
;;
;; When `result` is #f, fetch and normalise Python's pending exception
;; and dispatch by its class:
;;
;;   - `StopIteration`               → return the symbol 'StopIteration
;;                                     so iterator drivers can branch
;;                                     without entering Racket's
;;                                     exception machinery.
;;   - `SystemExit`                  → call `(exit code)` directly,
;;                                     mirroring how Python embeddings
;;                                     conventionally honour sys.exit.
;;   - `KeyboardInterrupt`           → raise `exn:break:pyffi:python`
;;                                     so broad `exn:fail?` handlers
;;                                     don't swallow user interrupts.
;;   - any other `Exception` (or
;;     `BaseException` falling through
;;     above)                        → raise `exn:fail:pyffi:python`,
;;                                     a subtype of `exn:fail` that
;;                                     existing `with-handlers` clauses
;;                                     catch transparently.
;;
;; In every raise case the live Python class and value are stored on
;; the resulting exn so consumers can introspect attributes, re-raise
;; back into Python, etc.  See `structs.rkt` for the full hierarchy.
(define-syntax (handle-python-exception stx)
  (syntax-parse stx
    [(_ qualified-name result:id)
     (syntax/loc stx
       (handle-python-exception qualified-name result #:wrap (λ (x) x)))]
    [(_ qualified-name result:id #:wrap wrap-fn:expr)
     (syntax/loc stx
       (let ()
         (define (pystring->string x) (PyUnicode_AsUTF8 (obj-the-obj x)))
         (define (matches? cls type-ptr)
           (and cls type-ptr
                (not (zero? (PyErr_GivenExceptionMatches cls type-ptr)))))
         (cond
           [result (wrap-fn result)]
           [else
            (define-values (ptype pvalue ptraceback) (PyErr_Fetch))
            (define-values (ntype nvalue ntraceback) (PyErr_NormalizeException ptype pvalue ptraceback))
            (define type-name
              (and ntype
                   (let ([n (PyObject_GetAttrString ntype "__name__")])
                     (and n (PyUnicode_AsUTF8 n)))))
            (define class-obj (and ntype (obj "type" ntype)))
            (define value-obj (obj (or type-name "Exception") nvalue))
            (define msg (let ([args (flat-vector->py-tuple (vector ntype nvalue))])
                          (py-list->list/pr (PyObject_Call py-format-exception-only args #f))))
            (define tb  (and ntraceback
                             (let ([args (flat-vector->py-tuple (vector ntraceback))])
                               (py-list->list/pr (PyObject_Call py-format-traceback args #f)))))
            (cond
              ;; StopIteration: signal-only sentinel.
              [(matches? ntype PyExc_StopIteration)
               'StopIteration]
              ;; SystemExit: honour sys.exit-style termination.
              ;; Python's convention: None → 0; int → that code; anything
              ;; else → 1 (Python additionally writes str(value) to
              ;; stderr; we leave that to the caller to add if wanted).
              [(matches? ntype PyExc_SystemExit)
               (define code-obj (and nvalue (PyObject_GetAttrString nvalue "code")))
               (define code
                 (cond
                   [(or (not code-obj) (equal? code-obj the-None)) 0]
                   [else
                    (define n (PyLong_AsLong code-obj))
                    (cond
                      [(and (= n -1) (PyErr_Occurred)) (PyErr_Clear) 1]
                      [else n])]))
               (exit code)]
              [else
               (define full-msg
                 (~a qualified-name ": " "Python exception occurred" ";\n"
                     (string-append* (map (λ (m) (~a " " (pystring->string m))) msg))
                     (if tb (string-append* (map (λ (m) (~a " " m)) tb)) "")))
               (cond
                 ;; KeyboardInterrupt: surface as exn:break:pyffi:python.
                 ;; exn:break's continuation field is contract-checked
                 ;; as escape-continuation?, so we capture one with
                 ;; call/ec at the raise site.  Invoking it (the user's
                 ;; "resume" hook) simply returns from this call/ec,
                 ;; which is the closest Racket analogue to "skip the
                 ;; break and continue past the failed Python call".
                 [(matches? ntype PyExc_KeyboardInterrupt)
                  (call/ec
                   (λ (resume)
                     (raise (exn:break:pyffi:python full-msg
                                                    (current-continuation-marks)
                                                    resume
                                                    class-obj value-obj
                                                    (or type-name "KeyboardInterrupt")
                                                    tb))))]
                 ;; Everything else (Exception subclasses, plus the rare
                 ;; BaseException-but-not-Exception cases like
                 ;; GeneratorExit and BaseExceptionGroup): exn:fail:pyffi:python.
                 [else
                  (raise (exn:fail:pyffi:python full-msg
                                                (current-continuation-marks)
                                                class-obj value-obj
                                                (or type-name "Exception")
                                                tb))])])])))]))

(define (pr/string x)
  (define has-name? (PyObject_HasAttrString (PyObject_Type x) "__name__"))
  (define name (and has-name? (PyUnicode_AsUTF8 (PyObject_GetAttrString (PyObject_Type x) "__name__"))))
  (case name
    [("str") (py-string->string x)]
    [else    (pr x)]))

(define (pr/key x)
  (define has-name? (PyObject_HasAttrString (PyObject_Type x) "__name__"))
  (define name (and has-name? (PyUnicode_AsUTF8 (PyObject_GetAttrString (PyObject_Type x) "__name__"))))
  (case name
    [("str")   (py-string->string x)]
    [("tuple") (py-tuple->vector x)]    
    [else      (pr x)]))


(define (pr x)
  ;; Convert a Python value to a Racket value.
  ;; The name `pr` stands for _P_ython to _R_acket.
  ;; Atomic values (numbers, booleans, None etc) are converted.
  ;; Non-atomic values (strings, dicts, lists, objects, functions etc)
  ;; are wrapped in an `obj` struct.
  (unless x (error 'pr "got false"))
  (cond
    ; Some functions in the C-api return Racket numbers; keep them.
    [(number? x) x]
    [else
     (define has-name? (PyObject_HasAttrString (PyObject_Type x) "__name__"))
     (cond
       [has-name? (define name      (PyUnicode_AsUTF8 (PyObject_GetAttrString (PyObject_Type x) "__name__")))
                  ; (displayln name)
                  (case name
                    [("int")       (py-int->number x)]
                    [("bool")      (if (= (PyObject_IsTrue x) 1) #t #f)]
                    [("float")     (py-float->number x)]
                    [("complex")   (py-complex->number x)]
                    ; [("function") (obj "function" x)]
                    [("int64")     (py-int->number x)]
                    [("NoneType")  (void)]
                    [("generator") (generator-obj name x)]
                    [else
                     (cond
                       [(= (PyCallable_Check x) 1)
                        (callable-obj name x
                                         (make-keyword-procedure
                                          (lambda (kws kw-args . as)
                                            ; (displayln (list 'here kws kw-args as))
                                            (define args
                                              (let ()
                                                (define n (length as))
                                                (define t (PyTuple_New n))
                                                (for ([a (in-list as)] [i (in-range n)])
                                                  (define v (rp a))
                                                  (Py_IncRef v) ; since PyTuple_SetItem steals reference
                                                  (case (PyTuple_SetItem t i v)
                                                    [(0)  (void)] ; succes
                                                    [else (error 'callable-obj "error in call to PyTuple_SetItem")]))
                                                t))
                                            
                                            (define kwargs (PyDict_New))
                                            (for ([kw    (in-list kws)]
                                                  [kwarg (in-list kw-args)])
                                              (PyDict_SetItemString kwargs (keyword->string kw) (rp kwarg)))
                                            
                                            (define result (PyObject_Call x args kwargs))
                                            (if result
                                                (pr result)
                                                (handle-python-exception 'callable-object result)))
                                          (λ as
                                            (define args
                                              (let ()
                                                (define n (length as))
                                                (define t (PyTuple_New n))
                                                (for ([a (in-list as)] [i (in-range n)])
                                                  (define v (rp a))
                                                  (Py_IncRef v) ; since PyTuple_SetItem steals reference
                                                  (case (PyTuple_SetItem t i v)
                                                    [(0)  (void)] ; succes
                                                    [else (error 'callable-obj "error in call to PyTuple_SetItem")]))
                                                t))
                                            (define kwargs (PyDict_New))
                                            (define result (PyObject_Call x args kwargs))
                                            (if result
                                                (pr result)
                                                (handle-python-exception 'callable-object result)))))]
                       [(equal? name "method")
                        (method-obj name x (λ as
                                             (apply PyObject_CallMethodObjArgs
                                                    (cons x (map rp as)))))]
                       [else
                        (obj name x)])])]
       [else #f])]))

(define (py-string? x)
  (and (obj? x)
       (let* ([o (obj-the-obj x)]
              [has-name? (PyObject_HasAttrString (PyObject_Type o) "__name__")])
         (and has-name?
              (PyUnicode_AsUTF8 (PyObject_GetAttrString (PyObject_Type o) "__name__"))))))


(define (python->racket x)
  (unless x (error 'python->racket "got false"))
  (cond
    [(number? x) x]
    [else
     (define has-name? (PyObject_HasAttrString (PyObject_Type x) "__name__"))
     (cond
       [has-name? (define name      (PyUnicode_AsUTF8 (PyObject_GetAttrString (PyObject_Type x) "__name__")))
                  (case name
                    [("str")     (py-string->string x)]
                    [("tuple")   (void "Todo: cache result")
                                 (define n (PyTuple_Size x))
                                 (define v (make-vector n))
                                 (for ([i (in-range n)])
                                   (vector-set! v i (python->racket (PyTuple_GetItem x i))))
                                 v]
                    [("ndarray") (py-ndarray->ndarray x)]
                    [else        (pr x)])])]))


#;(define (python->racket x)
  ; (displayln (list 'p->r x))
  ;; An easy way to see the type strings in the Python repl.
  ;;   >>> getattr(type(5.0),"__name__")
  ;;   'float'
  ; Note: We are assuming x is a non-cyclic data structure.
  (unless x (error 'python->racket "got false"))
  (cond
    [(number? x) x]
    [else
     ; (displayln (list 'pr 'x x (PyUnicode_AsUTF8 (PyObject_Str x))))
     (define has-name? (PyObject_HasAttrString (PyObject_Type x) "__name__"))
     (cond
       [has-name? (define name      (PyUnicode_AsUTF8 (PyObject_GetAttrString (PyObject_Type x) "__name__")))
                  ; (displayln name)
                  (case name
                    [("int")     (py-int->number x)]
                    [("float")   (py-float->number x)]
                    [("complex") (py-complex->number x)]
                    [("bool")    (if (= (PyObject_IsTrue x) 1) #t #f)]
                    
                    [("str")     (py-string->string x)]
                    [("tuple")   (void "Todo: cache result")
                                 (define n (PyTuple_Size x))
                                 (define v (make-vector n))
                                 (for ([i (in-range n)])
                                   (vector-set! v i (python->racket (PyTuple_GetItem x i))))
                                 v]
                    #;[("list")   (void "Todo: cache result")
                                (define n (PyList_Size x))
                                (for/list ([i (in-range n)])
                                  (python->racket (PyList_GetItem x i)))]
                    [("dict")   (void "Todo: Use PyDict_Next to iterate over the dict.")
                                (void "      Avoiding allocating new lists.")
                                (define items (PyDict_Items x)) ; Python list of (key,value) tuples
                                (define n (PyList_Size items))
                                (for/hash ([i (in-range n)])
                                  (define item (PyList_GetItem items i))
                                  (values (python->racket (PyTuple_GetItem item 0))
                                          (python->racket (PyTuple_GetItem item 1))))]
                    ;;; Numpy
                    [("ndarray")  (py-ndarray->ndarray x)]
                    ;; Numpy scalars
                    ;;   Numpy scalars have the same interface as `ndarray`.
                    ;;   We can use x.item() to get the value
                    ; [("bool_) ...]   

                    ; [("builtin_function_or_method") (obj "builtin_function_or_method" x)]
                    ; [("function" ) (obj "function" x)]
                    [("function" "method")   (pr x)]
                    [("int64")    (py-int->number x)]
                    [("NoneType") (void)]
                    
                    [else (obj name x)
                          #;(~a "<< " name " >>")])]
       [else #f])]))


;;;
;;; Python Types
;;;

; The goal of this module is to provide tools for importing Python functions to Racket.

; A `pytype` describes how to convert back and forth between a Python and Racket representation.
;     (struct pytype (type racket-to-python python-to-racket))

(define ~int      (pytype "int"     integer->py-int   py-int->number))
(define ~float    (pytype "float"   inexact->py-float py-float->number))
(define ~index    ~int)

(define ~tuple    (pytype "tuple"   vector->py-tuple  py-tuple->vector))
(define ~list     (pytype "list"    list->py-list     py-list->list))
(define ~string   (pytype "string"  string->py-string py-string->string))

(define ~py       (pytype "generic" racket->python    python->racket))
(define ~PY       (pytype "generic" rp                pr))
(define ~PY/FALSE (pytype "generic" rp                pr/false))

(define ~obj      (pytype "obj"     obj-the-obj       (λ (x) (obj "obj"  x))))

(define ~bool     (pytype "boolean" boolean->py-bool  py-bool->boolean))
(define ~None     (pytype "None"    #f #f))
(define ~char     (pytype "char"    (λ (c) (string->py-string (string c)))  (λ (x) (string-ref (py-string->string x) 0))))

(define ~NULL     (pytype "null"    (λ (_) #f)  (λ (_) #f)))



(define (ndarray->py-ndarray x) (obj-the-obj x))
(define (py-ndarray->ndarray x) (obj "ndarray" x))


#;(define (python-type x)
    (define type (PyObject_Type x))m
    (and (PyObject_HasAttrString                   type "__name__")
         (PyUnicode_AsUTF8 (PyObject_GetAttrString type "__name__"))))

;;;
;;; Conversion, Casting
;;; 


(define (convert val [type ~py]) ; to Racket value
  (cond
    [type (define to (pytype-python-to-racket type))
          (if to (to val) val)]
    [else  val]))

(define (cast val [type ~py]) ; to Python value
  (cond
    [type (define to (pytype-racket-to-python type))
          (if to (to val) val)]
    [else  val]))


(define (typename pyobj) (python-type pyobj))

;;;
;;; Procedures
;;;

; (struct pyprocedure (input-types output-type keywords keyword-types))

(define (~pyprocedure
         input-types    ; list of pytype
         output-type
         keywords       ; #f or list of symbols
         keyword-types  ; #f or list of ptype
         first-optional ; if non-#f, the index of the first optional argument
         )  ; pytype
  (when first-optional
    (unless (<= first-optional (length input-types))
      (error '~pyprocedure
             "the number of optional arguments is at most the number of input types")))
  (pyprocedure input-types output-type keywords keyword-types first-optional))

(define-syntax (~fun stx)
  (syntax-parse stx
    #:literals (->)
    [(_~fun (~and argument-type-spec (~not _:keyword)) ...
            (~seq kw:keyword kw-arg-type:expr) ... -> result-type-spec
            (~optional (~seq #:first-optional first-optional) #:defaults ([first-optional #'#f])))
     (define kw/kw-args (syntax->list #'((kw kw-arg) ...)))
     (cond
       [(eq? kw/kw-args '())
        (syntax/loc stx
          (~pyprocedure (list argument-type-spec ...)
                        result-type-spec
                        #f
                        #f
                        first-optional
                        ))]
       [else
        (syntax/loc stx
          (~pyprocedure (list argument-type-spec ...)
                        result-type-spec
                        (list 'kw         ...)
                        (list kw-arg-type ...)
                        first-optional))])]))
                        


;;; The form
;;;    (define-py id type-expr <option> ...)
;;; expands into 
;;;    1. A declaration of `id`.
;;;    2. Add an initialization thunk to the list `py-initialization-thunks`.
;;;       But only if the module isn't available now - if the module is available
;;;       get the value now.

;;; The problem is that we need access the Python module from which `id` is imported.
;;; But since `requires` run before anything else, at the time the Racket module
;;; containing `define-py` is run, the Python module might no be available yet.
;;; Therefore we make a dummy definition, and fill in the value later.

;;; In user code, use `define-delayed` or `define/delay` from "python-define-delayed.rkt".

(define empty-py-tuple #f)
(set! empty-py-tuple #f)
(define (set-empty-py-tuple! val) (set! empty-py-tuple val))


(define-syntax (define-py stx)
  (syntax-parse stx
    [(_define-py id:id type-expr:expr
                 (~or  (~optional (~seq #:prefix prefix) #:defaults ([prefix #'#f]))
                       (~optional (~seq #:from   from)   #:defaults ([from   #'#f])))
                 ...)
     (with-syntax ([id-to-get (if (syntax-e (attribute from))
                                  (string->symbol (~a (syntax-e (attribute from)) "." (syntax-e #'id)))
                                  #'id)]
                   [racket-id (if (syntax-e (attribute prefix))
                                  (format-id #'id "~a.~a" #'prefix #'id)
                                  #'id)]
                   [<uninitialized> (string->symbol (~a "uninitialized-define-py-function:" (syntax-e #'id) ">"))])
       (syntax/loc stx
         (begin
           (provide racket-id)
           (define racket-id '<uninitialized>) ; initialization delayed
           (define (init-thunk)
             (set-empty-py-tuple! (flat-vector->py-tuple #()))
             (set! racket-id
              (let ()
              (define py-obj (get 'id-to-get))
              (unless py-obj (error 'dont-use-dots))
              
              (define type   type-expr)
              (cond
                [(pyprocedure? type)
                 (define argument-types (pyprocedure-input-types type))
                 (define result-type    (pyprocedure-output-type type))
                 (define argn           (length argument-types))
                 (define to-rkt         (pytype-python-to-racket result-type))
                 (define keywords       (pyprocedure-keywords type))
                 (define keyword-types  (pyprocedure-keyword-types type))
                 (define first-optional (pyprocedure-optional-after type))
                 
                 (cond
                   ; The easy case with no keywords and no optional arguments
                   [(and (or (eq? keywords #f) (eq? keywords '()))
                         (eq? first-optional #f))
                    (case argn
                      [(0) (λ ()
                             (define result (PyObject_CallNoArgs py-obj))
                             (if to-rkt (to-rkt result) result))]
                     [(1) (let ()
                            (define to-py0 (pytype-racket-to-python (list-ref argument-types 0)))
                            (λ (a0)
                              (define v0 (if to-py0 (to-py0 a0) a0))
                              (define args (flat-vector->py-tuple (vector v0)))
                              (define kwargs #f)
                              (define result (PyObject_Call py-obj args kwargs))
                              (if to-rkt (to-rkt result) result)))]
                     [(2) (let ()
                            (define to-py0 (pytype-racket-to-python (list-ref argument-types 0)))
                            (define to-py1 (pytype-racket-to-python (list-ref argument-types 1)))
                            (λ (a0 a1)
                              (define v0 (if to-py0 (to-py0 a0) a0))
                              (define v1 (if to-py1 (to-py1 a1) a1))
                              (define args (flat-vector->py-tuple (vector v0 v1)))
                              (define kwargs #f)
                              (define result (PyObject_Call py-obj args kwargs))
                              (if to-rkt (to-rkt result) result)))]
                     [else (let ()
                             (define to-pys (map pytype-racket-to-python argument-types))
                             (λ as
                               (define args (flat-vector->py-tuple
                                             (for/vector #:length (length to-pys)
                                                 ([to-py (in-list to-pys)]
                                                  [a     (in-list as)])
                                               (if to-py (to-py a) a))))
                            (define kwargs #f)
                               (define result (PyObject_Call py-obj args kwargs))
                               (if to-rkt (to-rkt result) result)))])]
                  ; the case with keywords and no optional arguments
                   [(eq? first-optional #f)
                    
                   (let ()
                     (define to-pys    (map pytype-racket-to-python argument-types))
                     (define kw-to-pys (map pytype-racket-to-python keyword-types))
                     (make-keyword-procedure
                      (λ (kws kw-args . as)
                        ; todo: check that all keywords in kws are in `keywords`.
                        (define expected-n (length argument-types))
                        (define actual-n   (length as))
                        (unless (= expected-n actual-n)
                          (apply raise-arity-error 'id expected-n as))
                                 
                        (define kw-dict (PyDict_New))
                        (for ([kw       (in-list kws)]
                              [kw-arg   (in-list kw-args)]
                              [kw-to-py (in-list kw-to-pys)])
                          (define py-kw     (string->py-string (keyword->string kw)))
                          (define py-kw-arg (if kw-to-py (kw-to-py kw-arg) kw-arg))
                          (PyDict_SetItem kw-dict py-kw py-kw-arg))
                        
                        (define args  (flat-vector->py-tuple
                                       (for/vector #:length (length to-pys)
                                           ([to-py (in-list to-pys)]
                                            [a     (in-list as)])
                                         (if to-py (to-py a) a))))
                        ; (when args    (displayln (list 'args   (python->racket args))))
                        ; (when kw-dict (displayln (list 'kwargs (python->racket kw-dict))))
                        
                        (define result (PyObject_Call py-obj args kw-dict))
                        (if to-rkt (to-rkt result) result))))]
                  ; the case with optional arguments and no keyword arguments
                  ; todo: specialize for 0, 1 and 2 positional arguments
                   [(and (or (eq? keywords #f) (eq? keywords '()))
                        first-optional)
                    
                   (define to-pys (map pytype-racket-to-python argument-types))
                   (λ as
                     (when (< (length as) first-optional)
                       (error 'arity (~a "expected at least " first-optional " arguments")))
                     (when (> (length as) (length to-pys))
                       (apply raise-arity-error 'racket-id
                              (list (length to-pys) (arity-at-least first-optional)) as))
                     
                     (define args (flat-vector->py-tuple
                                   (for/vector #:length (length as)
                                       ([to-py (in-list to-pys)]
                                        [a     (in-list as)])
                                     (if to-py (to-py a) a))))
                     (define kwargs #f)
                     (define result (PyObject_Call py-obj args kwargs))
                     (if to-rkt (to-rkt result) result))]
                ; the case with optional arguments and keyword arguments
                ; todo: specialize for 0, 1 and 2 positional arguments
                   [first-optional
                    
                 (define to-pys (map pytype-racket-to-python argument-types))
                 (define kw-to-pys (map pytype-racket-to-python keyword-types))
                 (make-keyword-procedure
                 (λ (kws kw-args . as)
                   (when (< (length as) first-optional)
                     (error 'arity (~a "expected at least " first-optional " arguments")))
                   (when (> (length as) (length to-pys))
                     (apply raise-arity-error 'racket-id
                            (list (length to-pys) (arity-at-least first-optional)) as))
                   
                   (define args (flat-vector->py-tuple
                                 (for/vector #:length (length as)
                                     ([to-py (in-list to-pys)]
                                      [a     (in-list as)])
                                   (if to-py (to-py a) a))))
                   (define kw-dict (PyDict_New))
                   (for ([kw       (in-list kws)]
                         [kw-arg   (in-list kw-args)]
                         [kw-to-py (in-list kw-to-pys)])
                     (define py-kw     (string->py-string (keyword->string kw)))
                     (define py-kw-arg (if kw-to-py (kw-to-py kw-arg) kw-arg))
                     (PyDict_SetItem kw-dict py-kw py-kw-arg))
                   (define result (PyObject_Call py-obj args kw-dict))
                   (if to-rkt (to-rkt result) result)))]
                )]
                
               ; pyprocedure done
                [else
                 (define to-rkt (pytype-python-to-racket type))
                 (to-rkt py-obj)])))) ; end of init-thunk
           ; If our module is imported now, then get the value, otherwise delay.
           (if (id-bound? 'id-to-get)
               (init-thunk)
               (add-initialization-thunk init-thunk)))))]))

;;;
;;; Tell - Method Invocation
;;;

(define-syntax (tell stx)
  (syntax-parse stx
    #:literals (->)
    [(_tell obj-expr method:id (~seq arg-expr type-expr) ... -> result-type)
     (with-syntax ([method-str (symbol->string (syntax-e #'method))])
       (syntax/loc stx
         (let ()
           (define obj    obj-expr)
           (define pyobj  (if (obj? obj) (obj-the-obj obj) obj))
           (define args   (list arg-expr  ... #f))
           (define types  (list type-expr ... ~NULL))
           (define pyargs (for/list ([arg  (in-list args)]
                                     [type (in-list types)])
                            (define to (pytype-racket-to-python type))
                            (if to (to arg) arg)))
           ; (displayln (list 'meth method-str 'args args 'pyargs pyargs))
           ; todo: use a case expressions here to avoid the apply
           (define result (apply PyObject_CallMethodObjArgs
                                 pyobj
                                 (string->py-string method-str)
                                 pyargs))
           (define from (pytype-python-to-racket result-type))
           (if from (from result) result))))]))


