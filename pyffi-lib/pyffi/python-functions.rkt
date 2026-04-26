#lang racket/base
(provide get-fun
         get-fun-as-pyproc
         pyproc->procedure
         is-function?
         is-module?
         is-class?
         is-method?
         get-signature)

(require "structs.rkt"
         "python-c-api.rkt"
         "python-evaluation.rkt"
         "python-environment.rkt"
         "python-initialization.rkt"
         "python-delayed.rkt"
         ; "python-attributes.rkt"
         "python-types.rkt"
         "python-builtins.rkt"
         "python-operators.rkt"
         (only-in "python-list.rkt" pylist->list)
         racket/format
         racket/list
         racket/match
         racket/string)

(require (for-syntax racket/base
                     syntax/parse))

;;;
;;; Inspect
;;;

(define-py inspect.signature   (~fun ~obj -> ~py))
(define-py inspect.getmembers  (~fun ~py -> ~obj))

(define-py inspect.isfunction  (~fun ~obj -> ~py))
(define-py inspect.ismodule    (~fun ~obj -> ~py))
(define-py inspect.isclass     (~fun ~obj -> ~py))
(define-py inspect.ismethod    (~fun ~obj -> ~py))
(define-py inspect.isgenerator (~fun ~obj -> ~py))
; note: `getmembers` returns a cyclic data structure, so avoid using ~py

(define (is-function? x)
  (and (obj? x) (inspect.isfunction x)))

(define (is-module? x)
  (and (obj? x) (inspect.ismodule x)))

(define (is-class? x)
  (and (obj? x) (inspect.isclass x)))

(define (is-method? x)
  (and (obj? x) (inspect.ismethod x)))


(define (get-signature x)
  (and (obj? x) (inspect.signature x)))


;;;
;;; Procedures
;;;

(define (pyproc/no-keywords/no-positional-excess->procedure p)
  ; p      - a pyproc with the signature of pyfun
  
  ; Make a Racket procedure that calls the Python procedure described by p.
  ; The Python procedure has no keyword arguments.
  ; The Python procedure has no positional rest argument.

  (match-define (pyproc object name qualified-name
                        positional-parameters positional-types positional-excess 
                        keyword-parameters    keyword-types    keyword-excess
                        first-optional result-type) p)

  (define py-obj (obj-the-obj object))
  (define n_pos  (length positional-parameters))

  (define (identity x) x)
  (define (positional-to-converters)
    (apply values
            (for/list ([pt positional-types])
              (define to (and pt (pytype-racket-to-python pt)))
              (or to
                  racket->python))))       ; the default is the generic converter

  ; convert Python result to Racket value
  (define from-result (or (and result-type (pytype-python-to-racket result-type))
                          python->racket)) ; the default is the generic converter

  ;; The shared `handle-python-exception` macro (defined once in
  ;; python-types.rkt) applies `from-result` to a successful result via
  ;; `#:wrap from-result` and dispatches a Python exception to the
  ;; appropriate Racket exception type — see python-types.rkt and
  ;; structs.rkt for the full hierarchy.


  (define python-procedure
    (case n_pos
      [(0) (λ ()
             (define result (PyObject_CallNoArgs py-obj)) ; new reference
             (handle-python-exception qualified-name result #:wrap from-result))]
      [(1) (define to-py0 (positional-to-converters))
           (case first-optional
             [(#f)
              (λ (a0)                          
                (define v0     (to-py0 a0))
                (define args   (flat-vector->py-tuple (vector v0)))
                (define kwargs #f)
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))]
             [(0)
              (case-lambda
                [()
                 (define args   (flat-vector->py-tuple (vector)))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0)
                 (define v0     (to-py0 a0))
                 (define args   (flat-vector->py-tuple (vector v0)))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)])])]
                
      [(2) (define-values (to-py0 to-py1) (positional-to-converters))
           (case first-optional
             [(#f)
              (λ (a0 a1)
                (define v0     (to-py0 a0))
                (define v1     (to-py1 a1))
                (define args   (flat-vector->py-tuple (vector v0 v1)))
                (define kwargs #f)
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))]
             [(0)
              (case-lambda
                [()
                 (define args   (flat-vector->py-tuple (vector)))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0)
                 (define v0     (to-py0 a0))
                 (define args   (flat-vector->py-tuple (vector v0)))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0 a1)
                 (define v0     (to-py0 a0))
                 (define v1     (to-py1 a1))
                 (define args   (flat-vector->py-tuple (vector v0 v1)))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)])]
             [(1)
              (case-lambda
                [(a0)
                 (define v0     (to-py0 a0))
                 (define args   (flat-vector->py-tuple (vector v0)))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0 a1)
                 (define v0     (to-py0 a0))
                 (define v1     (to-py1 a1))
                 (define args   (flat-vector->py-tuple (vector v0 v1)))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)])])]
      [else (define tos (for/list ([pt positional-types])
                          (define to (and pt (pytype-racket-to-python pt)))
                          (or to racket->python))) ; generic default
            ; todo: reduce arity
            (λ as
              (define args (flat-vector->py-tuple
                            (for/vector #:length (length as)
                                ([to (in-list tos)] [a (in-list as)])
                              (to a))))
              (define kwargs #f)
              (define result (PyObject_Call py-obj args kwargs)) ; new reference
              (handle-python-exception qualified-name result #:wrap from-result))]))
  
  (procedure-rename python-procedure (string->symbol name)))

(define (pyproc/no-keywords/positional-excess->procedure p)
  ; p      - a pyproc with the signature of pyfun
  
  ; Make a Racket procedure that calls the Python procedure described by p.
  ; The Python procedure has no keyword arguments.
  ; The Python procedure has a positional rest argument.
  (match-define (pyproc object name qualified-name
                        positional-parameters positional-types positional-excess
                        keyword-parameters    keyword-types    keyword-excess
                        first-optional result-type) p)
  (unless positional-excess
    (error 'pyproc/no-keywords/positional-excess->procedure
           "expected a pyproc with an positional excess parameter"))

  (define py-obj (obj-the-obj object))
  (define n_pos  (length positional-parameters))

  (define (identity x) x)
  (define (positional-to-converters)
    (apply values
            (for/list ([pt positional-types])
              (define to (and pt (pytype-racket-to-python pt)))
              (or to
                  racket->python))))       ; the default is the generic converter

  ; convert Python result to Racket value
  (define from-result (or (and result-type (pytype-python-to-racket result-type))
                          python->racket)) ; the default is the generic converter

  ;; (Shared `handle-python-exception` macro lives in python-types.rkt;
  ;; see comment at the corresponding site in pyproc/no-keywords/no-positional-excess->procedure.)


  (define python-procedure
    (case n_pos
      [(0) (define to racket->python)
           (λ as ; only positional excess parameters
             (define args   (flat-vector->py-tuple (list->vector (map to as))))
             (define kwargs #f)
             (define result (PyObject_Call py-obj args kwargs)) ; new reference
             (handle-python-exception qualified-name result #:wrap from-result))]
      [(1) (define to racket->python)
           (define to-py0 (positional-to-converters))
           (case first-optional
             [(#f)
              (λ (a0 . as) ; 1 positional, then positional excess parameters
                (define v0     (to-py0 a0))
                (define args   (flat-vector->py-tuple (list->vector (cons v0 (map to as)))))
                (define kwargs #f)
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))]
             [(0)
              (case-lambda
                [()      ; optional positional not given
                 (define args   (flat-vector->py-tuple (list->vector #())))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0)     ; 1 positional
                 (define v0     (to-py0 a0))
                 (define args   (flat-vector->py-tuple #()))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0 . as) ; 1 positional, then positional excess parameters
                 (define v0     (to-py0 a0))
                 (define args   (flat-vector->py-tuple (list->vector (cons v0 (map to as)))))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)])]
             [else (error)])]
      [(2) (define to racket->python)
           (define-values (to-py0 to-py1) (positional-to-converters))
           (case first-optional
             [(#f)
              (λ (a0 a1 . as) ; 2 positional, then positional excess parameters
                (define v0     (to-py0 a0))
                (define v1     (to-py1 a1))
                (define args   (flat-vector->py-tuple (list->vector (cons v0 (cons v1 (map to as))))))
                (define kwargs #f)
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))]
             [(0)
              (case-lambda
                [()      ; optional positionals not given
                 (define args   (flat-vector->py-tuple (list->vector #())))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0)     ; 1 positional
                 (define v0     (to-py0 a0))
                 (define args   (flat-vector->py-tuple #()))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0 a1)  ; 2 positionals
                 (define v0     (to-py0 a0))
                 (define v1     (to-py1 a1))
                 (define args   (flat-vector->py-tuple (list->vector (list v0 v1))))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0 a1 . as) ; 2 positionals, then positional excess parameters
                 (define v0     (to-py0 a0))
                 (define v1     (to-py1 a1))
                 (define args   (flat-vector->py-tuple (list->vector (cons v0 (cons v1 (map to as))))))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)])]
             [(1)
              (case-lambda
                [(a0)     ; 1 positional
                 (define v0     (to-py0 a0))
                 (define args   (flat-vector->py-tuple #()))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0 a1)  ; 2 positionals
                 (define v0     (to-py0 a0))
                 (define v1     (to-py1 a1))
                 (define args   (flat-vector->py-tuple (list->vector (list v0 v1))))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)]
                [(a0 a1 . as) ; 2 positionals, then positional excess parameters
                 (define v0     (to-py0 a0))
                 (define v1     (to-py1 a1))
                 (define args   (flat-vector->py-tuple (list->vector (cons v0 (cons v1 (map to as))))))
                 (define kwargs #f)
                 (define result (PyObject_Call py-obj args kwargs)) ; new reference
                 (handle-python-exception qualified-name result #:wrap from-result)])])]
      [else (define to racket->python)
            (define tos (for/list ([pt positional-types])
                          (define to (and pt (pytype-racket-to-python pt)))
                          (or to racket->python))) ; generic default
            ; change arity
            (λ as
              (define pos-args    (for/list ([to (in-list tos)] [a (in-list as)]) (to a)))
              (define excess-args (for/list ([a (in-list (drop as n_pos))])
                                    (to a)))
              (define args (flat-vector->py-tuple (list->vector (append pos-args excess-args))))
              (define kwargs #f)
              (define result (PyObject_Call py-obj args kwargs)) ; new reference
              (handle-python-exception qualified-name result #:wrap from-result))]))
  (procedure-rename python-procedure (string->symbol name)))

(define (pyproc/keywords p)
  ; p      - a pyproc with the signature of pyfun
  
  ; Make a Racket procedure that calls the Python procedure described by p.
  ; The Python procedure has no keyword arguments.
  ; The Python procedure has a positional rest argument.
  (match-define (pyproc object name qualified-name
                        positional-parameters positional-types positional-excess
                        keyword-parameters    keyword-types    keyword-excess
                        first-optional result-type) p)
  (unless (or (not (empty? keyword-parameters))
              keyword-excess)
    (error 'pyproc/keywords
           "expected a pyproc with at least one keyword parameter"))

  (define py-obj (obj-the-obj object))
  (define n_pos  (length positional-parameters))
  (define n_kw   (length keyword-parameters))

  (define (identity x) x)
  (define (positional-types-to-converters)
    (apply values
            (for/list ([pt positional-types])
              (define to (and pt (pytype-racket-to-python pt)))
              (or to
                  racket->python))))       ; the default is the generic converter
  (define (keyword-types-to-converters)    
    (apply values
            (for/list ([kt keyword-types])
              (define to (and kt (pytype-racket-to-python kt)))
              (or to
                  racket->python))))

  ; convert Python result to Racket value
  (define from-result (or (and result-type (pytype-python-to-racket result-type))
                          python->racket)) ; the default is the generic converter

  (define python-procedure/no-keywords
    (if positional-excess
        (pyproc/no-keywords/positional-excess->procedure p)
        (pyproc/no-keywords/no-positional-excess->procedure p)))

  ;; (Shared `handle-python-exception` macro lives in python-types.rkt;
  ;; see comment at the corresponding site in pyproc/no-keywords/no-positional-excess->procedure.)


  (define python-procedure/keywords
    (case n_pos
      [(0) (define to racket->python)
           (define kw+to-ht (for/hash ([kw  keyword-parameters]
                                         [kwt keyword-types])
                              (values (string->keyword kw)
                                      (or (and kwt (pytype-python-to-racket kwt))
                                          racket->python))))
           (λ (kws kw-args . as) ; only positional excess parameters
             (define args    (flat-vector->py-tuple (list->vector (map to as))))
             (define kwargs  (PyDict_New))                      ; new reference
             (for ([kw     (in-list kws)]
                   [kw-arg (in-list kw-args)])
               (define to        racket->python)
               (define py-kw     (string->py-string (keyword->string kw)))
               (define py-kw-arg (to kw-arg))
               (PyDict_SetItem kwargs py-kw py-kw-arg))
             (define result (PyObject_Call py-obj args kwargs)) ; new reference
             (handle-python-exception qualified-name result #:wrap from-result))]
      [(1) (define to       racket->python)
           (define to-py0   (positional-types-to-converters))
           (define kw+to-ht (for/hash ([kw  keyword-parameters]
                                       [kwt keyword-types])
                              (values (string->keyword kw)
                                      (or (and kwt (pytype-python-to-racket kwt))
                                          racket->python))))
           (case first-optional
             [(#f)
              (λ (kws kw-args a0 . as)  ; no optional, positional arguments
                (define v0     (to-py0 a0))
                (define args   (flat-vector->py-tuple (list->vector (cons v0 (map to as)))))
                (define kwargs  (PyDict_New))                      ; new reference
                (for ([kw     (in-list kws)]
                      [kw-arg (in-list kw-args)])
                  (define to        (hash-ref kw+to-ht kw (λ () racket->python)))
                  (define py-kw     (string->py-string (keyword->string kw)))
                  (define py-kw-arg (to kw-arg))
                  (PyDict_SetItem kwargs py-kw py-kw-arg))
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))]
             [(0)
              (λ (kws kw-args . as)    ; all positional arguments are optional
                ; when a function has both optional and excess positional argument
                ; the positional is alwyas used first before the excess
                (define n         (length as))
                (define args      (flat-vector->py-tuple
                                   (list->vector
                                    (case n
                                      [(0)  '()]
                                      [(1)  (list (to-py0 (car as)))]
                                      [else (cons (to-py0 (car as)) (map to (rest as)))]))))
                (define kwargs  (PyDict_New))                      ; new reference
                (for ([kw     (in-list kws)]
                      [kw-arg (in-list kw-args)])
                  (define to        (hash-ref kw+to-ht kw (λ () racket->python)))
                  (define py-kw     (string->py-string (keyword->string kw)))
                  (define py-kw-arg (to kw-arg))
                  (PyDict_SetItem kwargs py-kw py-kw-arg))
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))])]
             
      [(2) (define to racket->python)
           (define-values (to-py0 to-py1) (positional-types-to-converters))
           (define kw+to-ht (for/hash ([kw  keyword-parameters]
                                       [kwt keyword-types])
                              (values (string->keyword kw)
                                      (or (and kwt (pytype-python-to-racket kwt))
                                          racket->python))))
           (case first-optional
             [(#f)
              (λ (kws kw-args a0 a1 . as) ; only positional excess parameters
                (define v0     (to-py0 a0))
                (define v1     (to-py0 a1))
                (define args   (flat-vector->py-tuple (list->vector (cons v0 (cons v1 (map to as))))))
                (define kwargs  (PyDict_New))                      ; new reference
                (for ([kw     (in-list kws)]
                      [kw-arg (in-list kw-args)])
                  (define to        (hash-ref kw+to-ht kw (λ () racket->python)))
                  (define py-kw     (string->py-string (keyword->string kw)))
                  (define py-kw-arg (to kw-arg))
                  (PyDict_SetItem kwargs py-kw py-kw-arg))
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))]
             [(0)
              (λ (kws kw-args . as)    ; all positional arguments are optional
                ; when a function has both optional and excess positional argument
                ; the positional is alwyas used first before the excess
                (define n         (length as))
                (define args      (flat-vector->py-tuple
                                   (list->vector
                                    (case n
                                      [(0)  '()]
                                      [(1)  (list (to-py0 (car as)))]
                                      [(2)  (list (to-py0 (car as)) (to-py1 (car as)))]
                                      [else (cons (to-py0 (car as))
                                                  (cons (to-py1 (cadr as))
                                                        (map to (rest (rest as)))))]))))
                (define kwargs  (PyDict_New))                      ; new reference
                (for ([kw     (in-list kws)]
                      [kw-arg (in-list kw-args)])
                  (define to        (hash-ref kw+to-ht kw (λ () racket->python)))
                  (define py-kw     (string->py-string (keyword->string kw)))
                  (define py-kw-arg (to kw-arg))
                  (PyDict_SetItem kwargs py-kw py-kw-arg))
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))]
             [(1)
              (λ (kws kw-args a0 . as)    ; all positional arguments are optional
                ; when a function has both optional and excess positional argument
                ; the positional is alwyas used first before the excess
                (define n         (length as))
                (define args      (flat-vector->py-tuple
                                   (list->vector
                                    (case (+ n 1)
                                      [(1)  (list (to-py0 a0))]
                                      [else (cons (to-py0 a0)
                                                  (map to (rest as)))]))))
                (define kwargs  (PyDict_New))                      ; new reference
                (for ([kw     (in-list kws)]
                      [kw-arg (in-list kw-args)])
                  (define to        (hash-ref kw+to-ht kw (λ () racket->python)))
                  (define py-kw     (string->py-string (keyword->string kw)))
                  (define py-kw-arg (to kw-arg))
                  (PyDict_SetItem kwargs py-kw py-kw-arg))
                (define result (PyObject_Call py-obj args kwargs)) ; new reference
                (handle-python-exception qualified-name result #:wrap from-result))])]
             
      [else (define to racket->python)
            (define tos (for/list ([pt positional-types])
                          (define to (and pt (pytype-racket-to-python pt)))
                          (or to racket->python))) ; generic default
            (define kw+to-ht (for/hash ([kw  keyword-parameters]
                                         [kwt keyword-types])
                              (values (string->keyword kw)
                                      (or (and kwt (pytype-python-to-racket kwt))
                                          racket->python))))
            ; change arity
            (λ (kws kw-args . as)
              (define pos-args    (for/list ([to (in-list tos)] [a (in-list as)]) (to a)))
              (define excess-args (for/list ([a (in-list (drop as n_pos))])
                                    (to a)))
              (define args   (flat-vector->py-tuple (list->vector (append pos-args excess-args))))
              (define kwargs  (PyDict_New))                      ; new reference
              (for ([kw     (in-list kws)]
                    [kw-arg (in-list kw-args)])
                (define to        (hash-ref kw+to-ht kw (λ () racket->python)))
                (define py-kw     (string->py-string (keyword->string kw)))
                (define py-kw-arg (to kw-arg))
                (PyDict_SetItem kwargs py-kw py-kw-arg))
              (define result (PyObject_Call py-obj args kwargs)) ; new reference
              (handle-python-exception qualified-name result #:wrap from-result))]))

  
  (make-keyword-procedure python-procedure/keywords
                          (procedure-rename python-procedure/no-keywords (string->symbol name))))


(define (pyproc->procedure p)
  (define pos-params (pyproc-positional-parameters p))
  (define n-pos      (length pos-params))
  (define kw-params  (pyproc-keyword-parameters p))
  (define n-kw       (length kw-params))
  (define pos-ex     (pyproc-positional-excess p))
  (define kw-ex      (pyproc-keyword-excess p))
  
  (cond
    [(or (> n-kw 0) kw-ex)  (pyproc/keywords p)]
    [pos-ex                 (pyproc/no-keywords/positional-excess->procedure p)]
    [else                   (pyproc/no-keywords/no-positional-excess->procedure p)]))

(define (get-fun qualified-name)
  (pyproc->procedure
   (get-fun-as-pyproc qualified-name)))

(require (only-in "python-constants.rkt" False)
         (only-in ffi/unsafe ptr-equal?))
(define (get-fun-as-pyproc qualified-name) ; e.g. qualified-name = 'inspect.signature
  ; (displayln (list 'get-fun-as-pyproc qualified-name))
  (define object (obj "pyfun" (get qualified-name)))
  (define name   (getattr object "__name__"))
  (define s      (inspect.signature object))
  (define ps     (getattr s "parameters"))
  (define names  (pylist->list (builtins.list ps))) ; parameter names (strings)
  (define params (for/list ([name names])           ; list of Parameter objects
                   (getitem ps name)))
  
  (define positional-parameters '())
  (define keyword-parameters    '())
  (define positional-excess     #f)
  (define keyword-excess        #f)

  (for ([p params])
    (define empty            (getattr p "empty"))
    (define name             (and (PyObject_HasAttrString                   (obj-the-obj p) "name")
                                  (PyUnicode_AsUTF8 (PyObject_GetAttrString (obj-the-obj p) "name"))))
    (define (empty? x)
      (and (obj? x) (ptr-equal? (obj-the-obj x) (obj-the-obj empty))))
    
    (define default          (if (hasattr p "default")
                                 (cond
                                   [(void?  (getattr p "default")) 'None]
                                   [(empty? (getattr p "default")) 'Empty]
                                   [else    (getattr p "default")])
                                 #f))
    ; (displayln (list 'attribute-name name 'default default))
    (define annotation       (getattr p "annotation"))
    (define kind.description (getattr (getattr p "kind") "description"))
    ; Note: Since Racket doesn't allow arguments that can be both
    ;       positional and keyword at the same time.
    ;       We need to choose.
    (case kind.description
      [("positional-only")
       (set! positional-parameters (cons name positional-parameters))]
      [("positional or keyword")
       ; if the argument has a default value, we make it a keyword parameter
       ; if there is no default, we make it a positional parameter
       (if (eq? default 'Empty)
           (set! positional-parameters (cons name positional-parameters))
           (set! keyword-parameters    (cons name keyword-parameters)))]
      [("keyword-only")
       (set! keyword-parameters (cons name keyword-parameters))]
      [("variadic positional")
       (set! positional-excess name)]
      [("variadic keyword")
       (set! keyword-excess name)]
      [else
       (error 'get-fun "internal error - did Python get a new type of parameter?")]))
  
  (set!   positional-parameters (reverse positional-parameters))
  (set!   keyword-parameters    (reverse keyword-parameters))
  (define positional-types      (make-list (length positional-parameters) #f))
  (define keyword-types         (make-list (length keyword-parameters)    #f))
  (define first-optional        #f)
  (define result-type           #f)
  
  (pyproc object name qualified-name
          positional-parameters positional-types positional-excess 
          keyword-parameters    keyword-types    keyword-excess
          first-optional result-type))

#;(define (pyproc->pyprocedure p)
  (define input-types    (make-list (length (pyproc-positional-parameters p)) ~py))
  (define output-type    ~py)
  (define keywords       (map string->symbol (pyproc-keyword-parameters p)))
  (define keyword-types  (make-list (length (pyproc-keyword-parameters p)) ~py))
  (define optional-after #f)
  (~pyprocedure input-types output-type keywords keyword-types optional-after))
