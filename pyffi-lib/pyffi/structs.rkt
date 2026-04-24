#lang racket/base
(require "parameters.rkt"
         racket/string
         racket/serialize)

(provide (struct-out pytype)
         (struct-out pyprocedure)
         (struct-out pyproc)

         (struct-out obj)
         (struct-out callable-obj)
         (struct-out method-obj)
         (struct-out generator-obj)
         (struct-out asis)

         ;; Exceptions
         (struct-out exn:fail:pyffi)
         (struct-out exn:fail:pyffi:not-configured))

;; ------------------------------------------------------------------
;; Exception hierarchy
;;
;; `exn:fail:pyffi`                — anything pyffi itself raises
;; `exn:fail:pyffi:not-configured` — pyffi's libpython / home preferences
;;                                   are unset.  Raised (instead of
;;                                   calling `(exit 1)`) so consumers
;;                                   can catch the condition and present
;;                                   a friendly message, fall back to a
;;                                   stub implementation, etc.
;; ------------------------------------------------------------------

(struct exn:fail:pyffi exn:fail () #:transparent)
(struct exn:fail:pyffi:not-configured exn:fail:pyffi () #:transparent)

(struct pytype (type racket-to-python python-to-racket))

(struct obj (type-name the-obj)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc obj port mode) (obj-print obj port mode))]
  #:property prop:serializable
  ; Note: The serialization doesn't serialize the Python object.
  ;       Serialization was added to support logging of results of examples in the manual,
  ;       but may not be needed at this point.
  (make-serialize-info
   (λ (this) (vector (obj-type-name this)))
   (cons 'obj-deserialize-info 'pyffi/structs)
   #f ; can cycle?
   (or (current-load-relative-directory) (current-directory)))
  )

(provide obj-deserialize-info)
(define obj-deserialize-info
  (make-deserialize-info
   ; make
   (λ (type-name)
     (obj type-name #f))
   ; cycle make
   (λ () 'todo)))

(struct callable-obj obj (app)
  #:property prop:procedure (struct-field-index app))

(struct method-obj obj (app)
  #:property prop:procedure (struct-field-index app))

(struct generator-obj obj ()
  #:property prop:sequence (λ(gen) ((current-pygenerator-prop:sequence) gen)))

; unsafe-struct*-ref(struct-field-index gen)

(struct pyprocedure (input-types output-type keywords keyword-types optional-after) #:transparent)


; This describes the parameters of a Python function
(struct pyproc (object name qualified-name
                positional-parameters positional-types positional-excess
                keyword-parameters    keyword-types    keyword-excess
                first-optional result-type)
  #:transparent)


(define (obj-print obj port mode)
  ; Note:
  ;   Called by the repr() built-in function to compute the “official”
  ;   string representation of an object. If at all possible, this
  ;   should look like a valid Python expression that could be used to
  ;   recreate an object with the same value (given an appropriate
  ;   environment).

  ;   Called by str(object) and the built-in functions format() and
  ;   print() to compute the “informal” or nicely printable string
  ;   representation of an object.
  ; Conclusion:
  ;   Use __repr__ for `write` and __str__ for `display`.
  (define repr (current-repr))
  (define str  (current-str))
  (when mode (write-string "(obj " port))
  (when (callable-obj?  obj) (write-string "callable "  port))
  (when (method-obj?    obj) (write-string "method "    port))
  (when (generator-obj? obj) (write-string "generator " port))
  (let ([tn (obj-type-name obj)]
        [o  (obj-the-obj obj)]
        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))])])
    (cond 
      [o (when mode (write tn         port)) ; write
         (when mode (display " : "    port)) ; write
         (define r (if mode (repr obj) (str obj)))
         (when (string-contains? r "\n")
           (newline port))
         (display r port)]
      [else
       (display tn port)]))
  (when mode (write-string ")" port)))

; Note: Is the name of the positional excess parameter important

; Note: Keyword paramers with no default value.
                           

; positional arguments
; keyword arguments

; When one or more parameters have the form parameter = expression,
; the function is said to have “default parameter values.” 
; For a parameter with a default value, the corresponding argument
; may be omitted from a call, in which case the parameter’s default value is substituted.

; If a parameter has a default value, all following parameters up until the “*” must
; also have a default value — this is a syntactic restriction that is not expressed by the grammar.

;; Function call semantics are described in more detail in section
;; Calls. A function call always assigns values to all parameters
;; mentioned in the parameter list, either from positional arguments,
;; from keyword arguments, or from default values.

;; If the form “*identifier” is present, it is initialized to a tuple
;; receiving any excess positional parameters, defaulting to the empty
;; tuple.

;; If the form “**identifier” is present, it is initialized to a new
;; ordered mapping receiving any excess keyword arguments, defaulting to
;; a new empty mapping of the same type.

;; Parameters after “*” or “*identifier” are keyword-only parameters and
;; may only be passed by keyword arguments.

;; Parameters before “/” are positional-only parameters and may only
;; be passed by positional arguments.


; Used by the manual to serialize results from examples.
(serializable-struct asis (s)
  #:property prop:custom-write
  (lambda (v p m) (display (asis-s v) p)))


