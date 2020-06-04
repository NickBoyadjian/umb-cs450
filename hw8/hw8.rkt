#lang errortrace racket
(require "hw8-util.rkt")

(provide (all-defined-out))

;; Utility function that converts a variable into a string
;; Useful when translating from SimpleJS into LambdaJS
(define (mk-field x)
  (match x [(s:variable x) (k:string (symbol->string x))]))

;; Utility function that allocates a j:object.
;; (mk-object) allocates an empty object
;; (mk-object (cons "foo" (k:number 1)) (cons "bar" (j:variable 'x)))
;;  allocates an object with one field "foo" and one field "bar"
(define/contract (mk-object . args)
  (-> (cons/c string? j:expression?) ... j:alloc?)
  (define (on-elem pair)
    (cons (k:string (car pair)) (cdr pair)))
  (j:alloc (j:object (make-immutable-hash (map on-elem args)))))

;;;;;;;;;;;;;;;
;; Exercise 1

(define/contract (translate exp)
  (-> s:expression? j:expression?)
  (match exp
    [(? k:const? k) k]
    [(s:variable x) (j:variable x)]
    [(s:let (s:variable x) s1 s2)
     (j:let (j:variable x) (translate s1) (translate s2))]
    [(s:apply f ea) (j:apply (translate f) (map translate ea))]
    [(s:load x y) (j:get (j:deref (translate x)) (mk-field y))]
    [(s:assign x y e) (mk-let (translate e) 
      (lambda (data) (mk-let (j:deref (translate x)) (lambda (o) (j:seq
          (j:assign (translate x) (j:set o (mk-field y) data))
          data)))))]
    [(s:function xs e) (j:alloc (j:object (hash
      (k:string "$code") (j:lambda (cons (j:variable 'this) (map translate xs)) (translate e))
      (k:string "prototype") (j:alloc (j:object (hash))))))]
    
    [(s:new ef es) 
      (mk-let (j:deref (translate ef)) 
        (lambda (ctor) 
        (mk-let (j:alloc (j:object (hash (k:string "$proto") (j:get ctor (k:string "prototype"))))) 
          (lambda (obj) (mk-let (j:get ctor (k:string "$code")) (lambda (f) (j:seq
            (j:apply (j:get ctor (k:string "$code")) (cons obj (map translate es)))
            obj )))))))]
    
    [(s:invoke x y es) (mk-let (j:get (j:deref (translate x)) (mk-field y)) (lambda (m)
      (mk-let (j:get (j:deref m) (k:string "$code")) (lambda (f)
        (j:apply f (cons (translate x) (map translate es)))
      ))
    ))]
    ))

(define desugar #f)

