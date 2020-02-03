#lang racket

; 3.14159 * (10 * 10)
(* 3.14159 (* 10 10))


; ((11*15) + (14+4)) + (3/9 - (14*3))
(+ 
    (+ 
        (* 11 15) ; this gets evaluated first. top to bottom left to right
        (+ 14 4)) 
    (- 
        (/ 9 3) 
        (* 14 3)))

; true and false are represented with #t and #f
(or #t #f) ; -> #t
(and #t #f) ; -> #f
; (and exp1 exp2) => if exp1 is false, exp2 wont be run

; if then else
(cond 
    [(> 5 3) 100] ; if x > 3 return 100
    [(> 5 1) 200] ; else if x > 1 return 200
    [else 300])   ; else return 300

; define variables
(define pi 3.14159) ; define evaluates to void
pi
