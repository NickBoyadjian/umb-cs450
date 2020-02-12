#lang racket
(require rackunit)
;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

(define ex1 (- (+ 15 (* 2 1)) (+(- 9 8) 4)))
(define ex2 
    (list
        (- (+ 15 (* 2 1)) (+(- 9 8) 4))
        (- (+ 15 2) (+(- 9 8) 4))
        (- 17 (+(- 9 8) 4))
        (- 17 (+ 1 4))
        (- 17 5)
        12))

(define (ex3 x y) (<= (+ (+ x 12) (* 4 13)) (- (+ x 11) y)))

;; Constructs a tree from two trees and a value
(define (tree left value right) (list left value right))

;; Constructs a tree with a single node
(define (tree-leaf value) (tree null value null))

;; Accessors
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value) (tree (tree-left self) value (tree-right self)))
(define (tree-set-left self left) (tree left (second self) (third self)))
(define (tree-set-right self right) (tree (first self) (second self) right))

;; Function that inserts a value in a BST
(define (bst-insert self value) (cond 
    [(null? self)                       (tree null value null)]
    [(equal? value (tree-value self))   (tree-set-value self value)]
    [(< value (tree-value self))        (tree-set-left self (bst-insert (tree-left self) value))]
    [else                               (tree-set-right self (bst-insert (tree-right self) value))]))



(define (non-symbols xs) (
    foldl (lambda (x res) (cond [(symbol? x) res] [else (+ res 1)])) 0 xs
))

;; lambda
(define (lambda? node) 
    (and 
        (list? node)                            ; is the node a list
        (>= (length node) 3)                    ; does is have  at least three elements
        (equal? (first node) 'lambda)           ; is the first word lambda
        (list? (second node))
        (equal? (non-symbols (second node)) 0)  ; are the arguments symbols
        (list? (second node))
        (or                                     ; return can either be (non empty list, number, symbol)
            (symbol? (third node))
            (and
                (list? (third node))
                (> (length (third node)) 0))
                (number? (third node))
            (number? (third node))
        )

    )
) 

(define (lambda-params node) (second node))

(check-equal? (list 'x) (lambda-params (quote (lambda (x) y))))

(define (lambda-body node) (drop node 2))

;; apply
(define (apply? l) 
    (and 
        (list? l)
        (>=(length l) 1)
        (not (equal? (first l) 'lambda))
    ))


(define (apply-func node) (first node))
(define (apply-args node) (drop node 1))

;; define
(define (define-basic? node) 
    (and
        (list? node)
        (>= (length node) 3)
        (equal? (first node) 'define)
        (symbol? (second node))
    ))

(define (define-func? node) (and
        (list? node)
        (>= (length node) 3)
        (equal? (first node) 'define)
        (list? (second node))
        (equal? (non-symbols (second node)) 0)
        (> (length (second node)) 0)
    )) 


(define (define? node) 
    (or
        (define-basic? node)
        (define-func? node)        
    ))