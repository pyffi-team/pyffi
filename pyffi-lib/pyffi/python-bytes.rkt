#lang racket/base

(provide (all-defined-out))

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-operators.rkt"
         "python-slice.rkt"
         "python-types.rkt")

(require racket/format)

(require (for-syntax racket/base syntax/parse racket/syntax))

;;;
;;; Byte Strings (Sequences of bytes)
;;;

(define (pybytes? x)
  (and (obj? x) (equal? (obj-type-name x) "bytes")))

(define (pybytes . bs)
  (unless (andmap byte? bs)
    (raise-arguments-error 'pybytes "expected bytes as input"
                           "bs" bs))
  (bytes->pybytes (apply bytes bs)))

(define (bytes->pybytes x)
  (unless (bytes? x) (error 'bytes->pybytes "got: ~a" x))
  (obj "bytes" (PyBytes_FromStringAndSize x (bytes-length x))))

(define (pybytes->bytes x)
  (unless (pybytes? x) (error 'pybytes->bytes "got: ~a" x))
  (PyBytes_AsStringAndSize (obj-the-obj x)))

(define (pybytes-length x)
  (unless (pybytes? x) (error 'pybytes-length "got: ~a" x))
  (PyObject_Length (obj-the-obj x)))

