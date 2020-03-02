#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^

;; Exercise 1.a: Read-write cell
; Solution has 3 lines.
(define (rw-cell x) (lambda (m) 
  (cond 
    [(equal? (list) m) x] ; get
    [else (rw-cell (first m))] ; set
  )))

;; Exercise 1.b: Read-only cell
;; Solution has 4 lines.
(define (ro-cell x) (lambda (m) 
  (cond 
    [(equal? (list) m) x] ; get
    [else (ro-cell x)] ; set
  )))


;; Exercise 2: Interperse
;; Solution has 11 lines.
; (define (intersperse l v)
;   (define (inner l res) 
;       (cond 
;         [(equal? (length l) 1) (append res l)]
;         [else (inner (rest l) (append res (list (first l)) (list v)))]))
  
;   (cond 
;     [(<= (length l) 1) l]
;     [else (inner l (list))])
; )

(define (intersperse l v) (cond
  [(<= (length l) 1) l]
  [else (rest 
          (foldl 
          (lambda (x res) (append res (list v x))) 
          (list) 
          l))
  ]
))

;; Exercise 3.a: Generic find
;; Solution has 7 lines.
(define (find pred l)
  (define (inner pred l index)
    (cond
      [(empty? l) #f]
      [(pred index (first l)) (cons index (first l))]
      [else (inner pred (rest l) (+ index 1))]
    ))
  (inner pred l 0))

;; Exercise 3.b: Member using find
;; Solution has 3 lines.
(define (member x l) 
  (cond
    [(equal? (find (lambda (idx elem) (equal? elem x)) l) #f) #f]
    [else #t]
  ))


;; Exercise 3.c: index-of using find
;; Solution has 4 lines.
(define (index-of l x) (cond
    [(member x l) (car (find (lambda (idx elem) (equal? elem x)) l))]
    [else #f]
  ))

;; Exercise 4: uncurry, tail-recursive
;; Solution has 8 lines.
(define (uncurry f) 
  (define (res f l) 
    (cond
      [(equal? l (list)) (f)]
      [(equal? 1 (length l)) (f (first l))]
      [else (res (f (first l)) (rest l) )]))
  (lambda (l) (res f l)))


;; Exercise 5: Parse a quoted AST
;; Solution has 26 lines.
(define (parse-ast node)

  (define (handle-list xs) (map parse-ast xs))

  (define (make-define-func node) (r:define
    (parse-ast (first (second node)))
    (r:lambda
      (handle-list (rest (second node)))
      (handle-list (rest (rest node)) )
    )
  ))

  (define (make-define-basic node) (r:define
    (parse-ast (second node))
    (parse-ast (third node))
  ))

  (define (make-lambda node) (r:lambda 
    (handle-list (second node))
    (handle-list (rest (rest node)))
  ))

  (define (make-apply node) (r:apply
    (parse-ast (first node))
    (handle-list (rest node))
  ))

  (define (make-number node) (r:number node))

  (define (make-variable node) (r:variable node))


  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
