#lang errortrace racket
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require rackunit)
(require "ast.rkt")
(provide (all-defined-out))

;; Exercise 1.a
(define p:empty (delay empty))

;; Exercise 1.b(
(define (p:empty? p) (and
  (promise? p)
  (equal? (force p) empty)
))

;; Exercise 1.d
(define (p:first l) (car (force l)))

;; Exercise 1.e
(define (p:rest l) (cdr (force l)))


;; Exercise 1.f

; turns a promise list into a regular list
(define (p:destruct in out) (cond
  [(p:empty? in) out]
  [else (p:destruct (p:rest in) (append out (list (p:first in))))]))

(define (p:append l1 l2) 
  (define l:l1 (p:destruct l1 empty))
  (define l:l2 (p:destruct l2 empty))
  (define nl (append l:l1 l:l2))
  (define (inner in) (
    cond 
      [(equal? (length in) 1) (delay (cons (first in) p:empty))]
      [else (delay (cons (first in) (inner (rest in))))]
  ))
  (inner nl))




;; Exercise 2.a
;; Auxiliary functions
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))
#|
(define (bst->list self)
  (cond [(empty? self) self]
        [else
         (append
           (bst->list (tree-left self))
           (cons (tree-value self)
                 (bst->list (tree-right self))))]))
|#
(define (bst->p:list self) (cond [(empty? self) p:empty]
        [else
         (p:append
           (bst->p:list (tree-left self))
           (delay (cons (tree-value self)
                 (bst->p:list (tree-right self)))))]))

;; Exercise 3
;; Auxiliary functions
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))
(define (stream-foldl f a s) 'todo)

;; Exercise 4
(define (stream-skip n s) 'todo)

;; Exercise 5
(define (r:eval-builtin sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [else #f]))

(define (r:eval-exp exp)
  (cond
    ; 1. When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ;    return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a function call evaluate each expression and apply
    ;    the first expression to remaining ones
    [(r:apply? exp)
     ((r:eval-exp (r:apply-func exp))
      (r:eval-exp (first (r:apply-args exp)))
      (r:eval-exp (second (r:apply-args exp))))]
    [else (error "Unknown expression:" exp)]))


(define r:bool #f)
(define r:bool-value #f)
(define r:bool? #f)
