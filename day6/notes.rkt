#lang racket

;; look up cse341 (this lecture is based off of it)

(define (countup x)
    ; here we nest our inner count function
    (define (count low) 
        (cond 
            [(= low x) (list low)]
            [else (cons low (count (+ low 1)))])) 
    
    ; and then we call it from one to x
    (count 1) ; the last definition in the function is what gets returned, thats why this comes last
)


(countup 11)




; given a list, find the maximum number in the list
(define (max-list xs) (
    foldl
        (lambda (x res) (cond [(> x res) x] [else res])) 
        (first xs) 
        xs
))

(max-list (list 1 2 8 4))

