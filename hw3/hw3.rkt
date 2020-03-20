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


;; 1.c
;; it is possible, you just need to wrap the x value with a delay(promise).
; (define (p:cons x l) (delay (cons x l)))
; (define test (p:cons (delay (+ 1 2)) p:empty))
; (check-equal? (force (p:first test)) 3)


;; Exercise 1.f

; turns a promise list into a regular list
(define (p:destruct in out) (cond
  [(p:empty? in) out]
  [else (p:destruct (p:rest in) (append out (list (p:first in))))]))

(define (p:append l1 l2) 
  (define nl (append (p:destruct l1 empty) (p:destruct l2 empty))) ; destruct and append the two lists
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

;; 2.b
; lazy evauation out-performs eager evaluation when you have a situation where something 
; that would normally be evaluate, can repeatedly be skipped. For example, in bst->p:list, 
; you never have to actually evaluate the value of the tree, you just delay that,and move 
; on to the next iteration of the function, where as the normal version of that function,
; you evaluate at each step. 

;; Exercise 3
;; Auxiliary functions
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))
(define (stream-foldl f a s)
  (define next-a (f (stream-get s) a))
  (f a (thunk (stream-foldl f next-a (stream-next s)))))


;; Exercise 4
(define (stream-skip n s) 
  (define (inner count s) (cond 
    [(equal? count n) s]
    [else (inner (+ count 1) (stream-next s))]))
  (inner 0 s))

;; Exercise 5
(struct r:bool (value) #:transparent)

(define (and-n l) (foldl (lambda (x y) (and y x)) #t l))
(define (plus l) (foldl + 0 l))


(define (r:eval-builtin sym)
  (cond [(equal? sym '+) plus]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [(equal? sym 'and) and-n]
        [else #f]))

(define (r:eval-exp exp)
  
  (cond
    [(r:bool? exp) (r:bool-value exp)]
    ; 1. When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ;    return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a function call evaluate each expression and apply
    ;    the first expression to remaining ones
    
    [(equal? (r:apply-func exp) (r:variable '+)) (plus (map (lambda (x) (r:eval-exp x)) (r:apply-args exp)))]
    [(equal? (r:apply-func exp) (r:variable 'and)) (and-n (map (lambda (x) (r:eval-exp x)) (r:apply-args exp)))]

    [(r:apply? exp)
      ((r:eval-exp (r:apply-func exp))
        (r:eval-exp (first (r:apply-args exp)))
        (r:eval-exp (second (r:apply-args exp))))]
    [else (error "Unknown expression:" exp)]))
