#lang racket/base

(require pyffi
         rackunit)

(initialize)
(finish-initialization)

(define mybytes (bytes->pybytes #"test\0bytes"))
(check-true (pybytes? mybytes))
(check-equal? (pybytes-length mybytes) 10)
(check-equal? (pybytes->bytes mybytes) #"test\0bytes")
