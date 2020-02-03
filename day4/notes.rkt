#lang racket
(require rackunit)

;; PAIRS
(define pair (cons (+ 1 2) (* 2 3))) ; cons is used to construct the pair
(car pair) ; car gets the first element
(cdr pair) ; cdr gets the second element

; create a function that takes a pair and flips the elements
(define (flip-pair pair) 
    (cons (cdr pair) (car pair)))

(flip-pair pair)

;; UNIT TESTS
(check-equal? (cons 2 1) (flip-pair (cons 1 2))) ; if this is true, which it is, nothing will happen
; (check-equal? (cons 2 1) (flip-pair (cons 2 2))) ; this one will fail and will tell us when we run the code

(define (pair+ p1 p2) 
    (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(check-equal? (cons 5 5) (pair+ (cons 4 4) (cons 1 1)))



;; LISTS
(define my-list (list 1 2 3 4 5))
(empty? (list))
(cdr my-list) ; gives the tail of the list
(first my-list)
(rest my-list)