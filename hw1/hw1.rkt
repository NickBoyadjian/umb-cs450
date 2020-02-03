#lang racket
(require racket/list)
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

(define ex3 (lambda (x y) (<= (+ (+ x 12) (* 4 13)) (- (+ x 11) y))))

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
    [(equal? self null) (tree null value null)]
    [(equal? value (tree-value self)) (tree-set-value self value)]
    [(< value (tree-value self)) (tree-set-left self (tree-leaf value))]
    [else (tree-set-right self (tree-leaf value))]))


;; lambda
(define (lambda? node) 'todo)
(define (lambda-params node) 'todo)
(define (lambda-body node) 'todo)

;; apply
(define (apply? l) 'todo)
(define (apply-func node) 'todo)
(define (apply-args node) 'todo)

;; define
(define (define? node) 'todo)
(define (define-basic? node) 'todo)
(define (define-func? node) 'todo)



