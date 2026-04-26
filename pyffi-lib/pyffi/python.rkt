#lang racket/base
(require "structs.rkt"
         "python-attributes.rkt"
         "python-builtins.rkt"
         "python-bytes.rkt"
         "python-c-api.rkt"
         "python-define-delayed.rkt"
         "python-dict.rkt"
         "python-environment.rkt"
         "python-evaluation.rkt"
         "python-functions.rkt"
         "python-generator.rkt"
         "python-initialization.rkt"
         "python-import.rkt"
         "python-list.rkt"
         "python-module.rkt"
         "python-more-builtins.rkt"
         "python-operators.rkt"
         "python-slice.rkt"
         "python-string.rkt"
         "python-tuple.rkt"
         "python-types.rkt")

(provide (except-out
          (all-from-out
           "structs.rkt"
           "python-attributes.rkt"
           "python-builtins.rkt"
           "python-bytes.rkt"
           "python-c-api.rkt"
           "python-define-delayed.rkt"
           "python-dict.rkt"
           "python-environment.rkt"
           "python-evaluation.rkt"
           "python-functions.rkt"
           "python-generator.rkt"
           "python-initialization.rkt"
           "python-import.rkt"
           "python-list.rkt"
           "python-module.rkt"
           "python-more-builtins.rkt"
           "python-operators.rkt"
           "python-slice.rkt"
           "python-string.rkt"
           "python-tuple.rkt"
           "python-types.rkt")
          ; the values are wrapped in an obj struct below
          builtins main
          
          ; The procedures run and run* return cpointers.
          ; Automatic `pr` conversion is provided below
          run run*))

(require "python-delayed.rkt")

;; Modules:  builtins, main


(define obj-builtins 'uninitialized-obj-builtins)
(define obj-main     'uninitialized-obj-main)

(add-initialization-thunk
 (λ ()
   (set! obj-builtins (obj "module" builtins))
   (set! obj-main     (obj "module" main))))

(provide (rename-out [obj-builtins builtins]
                     [obj-main     main]))

;; Automatic conversion:  run, run*
;;
;; `handle-python-exception`, the `py-format-*` parameters and their
;; initialisation thunks live in python-types.rkt.  This file picks up
;; the macro from there via the `python-types` import in the require
;; list above.

(define (prrun x)
  (define result (run x))
  (handle-python-exception 'run result)
  (pr result))

(define (prrun* x)
  (define result (run* x))  
  (handle-python-exception 'run* result)
  (void result))


(provide (rename-out [prrun  run]
                     [prrun* run*]))
