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
(require "hw5-util.rkt")
(require rackunit)
(provide d:eval-exp d:eval-term)
(define (d:apply-arg1 app)
  (first (d:apply-args app)))
(define (d:lambda-param1 lam)
  (first (d:lambda-params lam)))
;; END OF REQUIRES

;; Exercise 1
(define/contract (d:eval-exp mem env exp)
  (-> mem? handle? d:expression? eff?)
  (println exp)
  (define frame (heap-get mem env))
  (define local-vars (frame-locals frame))
  (cond
    [(d:value? exp) (eff mem exp)]
    [(d:variable? exp) (eff mem (environ-get mem env exp))]
    [(d:lambda? exp) (eff mem (d:closure env exp))]
    [(d:apply? exp) 
      (define closure (d:eval-exp mem env (d:apply-func exp)))
      (define closure-env (d:closure-env (eff-result closure)))
      (define closure-state (eff-state closure))
      (define closure-lambda (d:closure-decl (eff-result closure)))
      (define x (d:lambda-param1 closure-lambda))
      (define tb (d:lambda-body closure-lambda))

      (define va* (d:eval-exp closure-state env (d:apply-arg1 exp)))
      (define va (eff-result va*))
      (define Ef (eff-state va*))
      
      (define Eb* (environ-push Ef closure-env x va))      
      (define Eb (eff-state Eb*))
      (define new-env (eff-result Eb*))

      (d:eval-exp Eb new-env tb)
    ]
  ))


;; Exercise 2
(define/contract (d:eval-term mem env term)
  (-> mem? handle? d:term? eff?)
  (print "term")
  (println term)
  (cond
    [(d:define? term) 
      (define v (eff-result (d:eval-term mem env (d:define-body term))))

      (define new-mem (eff-state (d:eval-term mem env (d:define-body term))))
      (define x (d:define-var term))
      (println x)
      (define E (environ-put new-mem env x v))
      (eff E (d:void))
    ]
    [(d:seq? term)
      (define v1-state (eff-state (d:eval-term mem env (d:seq-fst term))))
      (define v2 (d:seq-snd term)) 
      (d:eval-term v1-state env v2)
    ]
    [else (d:eval-exp mem env term)]
  ))

;; Exercise 3 (Manually graded)
#|

|#
