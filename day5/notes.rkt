#lang racket
(require rackunit)

; defines the data structure for a point
(define (point x y z) (list x y z)) ; constructor
(define (point? p) (and (list? p) (equal? (length p) 3))) ; checks if object is a point 
(define (point-x p) (first p)) ; get x
(define (point-y p) (second p)) ; get y 
(define (point-z p) (third p)) ; get z

; test the data structure
(define mypoint (point 1 2 3))
(point? mypoint)
(point? (list 1 2 3))
(point? (list 1 2 3 4))

