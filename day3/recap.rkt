#lang racket

(define n 5)
(define (fizzbuzz n) (cond
    [(zero? (modulo n 15)) "fizzbuzz"] 
    [(zero? (modulo n 5)) "fizz"] 
    [(zero? (modulo n 15)) "buzz"]
    [else n]))

(fizzbuzz 3)

(define x (* 10 2)) ; first, 10 and 2 are multiplied, then the result is set to x.. so 2 evaluations here
(+ x (* 4 2)) ; first, x needs to be looked up, then 4 and 2 are muliplied, then the add is executed.
; How many evaluation steps should we expect... 5
