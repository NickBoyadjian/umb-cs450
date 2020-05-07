#lang errortrace racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at:

   https://www.umb.edu/life_on_campus/dean_of_students/student_conduct

|#
(require "hw7-util.rkt")
(provide (all-defined-out))

(define/contract (env-put env var val)
  (-> handle? d:variable? d:value? eff-op?)
  (eff-op (lambda (mem) (eff (environ-put mem env var val) (d:void)))))

(define/contract (env-push env var val)
  (-> handle? d:variable? d:value? eff-op?)
  (eff-op (lambda (mem) (environ-push mem env var val))))

(define/contract (env-get env var)
  (-> handle? d:variable? eff-op?)
  (eff-op (lambda (mem) (eff mem (environ-get mem env var)))))

(define/contract (d:eval-exp env exp)
  (-> handle? d:expression? eff-op?)
  (match exp
    [_ #:when (d:value? exp) (eff-pure exp)]
    [(d:variable _)   (env-get env exp)]
    [(d:lambda _ _)   (eff-pure (d:closure env exp))]
    [(d:apply (d:apply (d:apply (d:variable 'if) (list ec)) (list et)) (list ef)) (do
      v <- (d:eval-exp env ec)
      (match v
        [(d:bool #f) (d:eval-exp env ef)]
        [_ (d:eval-exp env et)])
    )]
    [(d:apply ef (list ea))  
     (do 
      closure <- (d:eval-exp env ef)
      (match closure
        [(d:closure closure-env (d:lambda (list x) tb)) 
         (do
          va <- (d:eval-exp env ea)
          Eb <- (env-push closure-env x va)
          vb <- ((d:eval-term-impl) Eb tb)
          (eff-pure vb)
        )]
        [(d:builtin f) (do 
          va <- (d:eval-exp env ea)
          (eff-pure (f va))
        )]))]
    [_ ((d:eval-term-impl) env exp)]
  ))



(define/contract (d:eval-term env term)
  (-> handle? d:term? eff-op?)
  (match term
    [(d:define x e) 
     (do
      v <- (d:eval-term env e)
      (env-put env x v))]   
    [(d:seq t1 t2) 
     (do
      v1 <- (d:eval-term env t1)
      v2 <- (d:eval-term env t2)
      (eff-pure v2))]
    [_ ((d:eval-exp-impl) env term)]))

;; Use this dynamic parameter in d:eval-term for improved testing (see Lecture 31)
(define d:eval-exp-impl (make-parameter d:eval-exp))
;; Use this dynamic parameter in d:eval-exp for improved testing (see Lecture 31)
(define d:eval-term-impl (make-parameter d:eval-term))

;; Parameter body *must* be a curried term already
(define/contract (break-lambda args body)
  (-> (listof d:variable?) d:term? d:lambda?)
  (match args
    [(list x)         (d:lambda args body)]
    [(list x xs ...)  (d:lambda (list x) (break-lambda xs body))]
    [_                (d:lambda (list (d:variable '_)) body)]))

;; ef is a curried expression and args is a list of curried expressions
(define/contract (break-apply ef args)
  (-> d:expression? (listof d:expression?) d:expression?)
  (match args
  [(list x)        (match x
    [(d:apply f a) (d:apply ef (list (break-apply f a)))]
    [_ (d:apply ef args)]
  )]
  [(list x xs ...) (break-apply (d:apply ef (list x)) xs)]
  [_               (d:apply ef (list (d:void)))]
  ))

;; Use this dynamic parameter in d:curry for improved testing (see Lecture 31)
(define break-lambda-impl (make-parameter break-lambda))
;; Use this dynamic parameter in d:curry for improved testing (see Lecture 31)
(define break-apply-impl (make-parameter break-apply))

(define/contract (d:curry term)
  (-> d:term? d:term?)
  (match term
    [(d:apply ef ea) ((break-apply-impl) (d:curry ef) (d:curry ea))]
    [(d:lambda p b) ((break-lambda-impl) (d:curry p) (d:curry b))]
    [(d:seq fst snd) (d:seq (d:curry fst) (d:curry snd))]
    [(d:define name body) (d:define (d:curry name) (d:curry body))]
    [_ term])
  )
