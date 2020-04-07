#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang errortrace racket
(provide (all-defined-out))
(require "hw4-util.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))
;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))


;; Exercise 1
(define (s:subst exp var val) (cond
  [(s:number? exp) exp]
  [(s:variable? exp) (cond 
    [(equal? exp var) val] 
    [else exp])]
  [(s:lambda? exp) (cond
    [(equal? (s:lambda-param1 exp) var) exp]
    [else (s:lambda (list (s:lambda-param1 exp)) (list (s:subst (s:lambda-body1 exp) var val)))])]
  [(s:apply? exp) 
    (s:apply 
      (s:subst (s:apply-func exp) var val) 
      (list (s:subst (s:apply-arg1 exp) var val)))]))


;; Exercise 2var

(define (s:eval subst exp) (cond
  [(s:value? exp) exp]
  [(s:apply? exp)
      (define ef (s:eval subst (s:apply-func exp)))
      (define va (s:eval subst (s:apply-arg1 exp)))
      (s:eval subst (subst
        (s:lambda-body1 ef) 
        (s:lambda-param1 ef) 
        va))
  ]))



;; Exercise 3
(define (e:eval env exp) (cond
  [(e:value? exp) exp]
  [(e:variable? exp) (hash-ref env exp)]
  [(e:lambda? exp) (e:closure env exp)]
  [(e:apply? exp)    
    (define clos (e:eval env (e:apply-func exp)))
    (define Eb (e:closure-env clos))
    (define eb (e:lambda-body1 (e:closure-decl clos)))
    (define x (e:lambda-param1 (e:closure-decl clos)))
    (define va (e:eval env (e:apply-arg1 exp)))
    (e:eval (hash-set Eb x va) eb)
  ]
))



;; Exercise 4 (Manually graded)
#|
Using environments is better when you want to have multiple variables or arguments
to your functions. It would make more sense not to use environments if you are 
working only with functions that take one argument (where you would using currying instead),
because then you wouldn't really need to bother keeping all your data in a hash table.
|#

;; Exercise 5 (Manually graded)
#|
The first major benefit is that it is a lot easier to read.
You don't have to bother looking at code and can instead 
understand the way it works in a more mathematical notation.
The other benefit to this is that you can leave the actual
implementation up to optimization and not focus on that when
designing the specification.
|#
