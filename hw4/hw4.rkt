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
(define (handle-apply-func exp var val) (cond
  [(equal? (s:apply-func exp) var) val]
  [else (s:apply-func exp)]
))

(define (handle-apply-args args var val) (map (lambda (x) (cond [(equal? x var) val] [else x])) args))

; (list (s:variable (quote y))

(define (s:subst exp var val)
(cond
  [(s:apply? exp) (s:apply (handle-apply-func exp var val) (handle-apply-args (s:apply-args exp) var val))]
  
  [(equal? exp var) val]
  [else exp]
)
)

;; Exercise 2var
(define (s:eval subst exp) 'todo)

;; Exercise 3
(define (e:eval env exp) 'todo)

;; Exercise 4 (Manually graded)
#|
PLEASE REPLACE THIS TEXT BY YOU ANSWER.
YOU MAY USE MULTIPLE LINES.
|#

;; Exercise 5 (Manually graded)
#|
PLEASE REPLACE THIS TEXT BY YOU ANSWER
YOU MAY USE MULTIPLE LINES.
|#
