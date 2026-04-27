#lang racket
(require pyffi)

(initialize)
(finish-initialization)

(define hw (string->pystring "Hello World"))

(write   hw) (newline)
(display hw) (newline)
