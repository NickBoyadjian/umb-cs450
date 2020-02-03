#lang racket

; Function declarations
; function-dec = (lambda (variable* ) term+)

(define  circumference (lambda (radius) (* 2 3.14159 radius)))
(circumference 2)

(define fst (lambda (a b) a))
(fst 2 5)

(define zero (lambda () 0))
(zero)

