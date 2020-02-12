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
(define (intersperse l v)
  (define (inner l res) 
      (cond 
        [(equal? (length l) 1) (append res l)]
        [else (inner (rest l) (append res (list (first l)) (list v)))]))
  
  (cond 
    [(equal? (length l) 1) l]
    [else (inner l (list))])
)


;; Exercise 3.a: Generic find
;; Solution has 7 lines.
(define (find pred l) 'todo)

;; Exercise 3.b: Member using find
;; Solution has 3 lines.
(define (member x l) 'todo)

;; Exercise 3.c: index-of using find
;; Solution has 4 lines.
(define (index-of l x) 'todo)

;; Exercise 4: uncurry, tail-recursive
;; Solution has 8 lines.
(define (uncurry f) 'todo)

;; Exercise 5: Parse a quoted AST
;; Solution has 26 lines.
(define (parse-ast node)
  (define (make-define-func node) 'todo)
  (define (make-define-basic node) 'todo)
  (define (make-lambda node) 'todo)
  (define (make-apply node) 'todo)
  (define (make-number node) 'todo)
  (define (make-variable node) 'todo)

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
