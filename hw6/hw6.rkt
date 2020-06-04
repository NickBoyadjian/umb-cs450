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
(require racket/set)
(require "hw6-util.rkt")
(provide frame-refs mem-mark mem-sweep mlist mapply)

;;;;;;;;;;;;;;;
;; Exercise 1

(define/contract (frame-refs frm)
  (-> frame? set?)
  (define folded 
    (frame-fold 
      (lambda (var val res) (cond [(d:closure? val) (set-add res (d:closure-env val))] [else res])) 
      (set) 
      frm))
  (cond
    [(frame-parent frm) (set-add folded (frame-parent frm))]
    [else folded]
  ))

;;;;;;;;;;;;;;;
;; Exercise 2

(define/contract (mem-mark contained mem env)
  (-> (-> any/c set?) heap? handle? set?)
  (define frm  (heap-get mem env))
  (define parent (frame-parent frm))

  (println (environ-get mem env (d:variable 'x)))

  
  (cond
    [(or (not parent) (equal? parent env)) (contained frm)]
    [else (set-add (set-union (contained frm) (mem-mark contained mem parent)) env)]
  ))

;;;;;;;;;;;;;;;
;; Exercise 3

(define/contract (mem-sweep mem to-keep)
  (-> heap? set? heap?)
  (heap-filter 
    (lambda (key value) (set-member? to-keep key))
    mem
  )
)

;;;;;;;;;;;;;;;
;; Exercise 4
(define (mlistinner bind pure args res) (cond
  [(equal? (length args) 0) (pure res)]
  [else (bind (first args) (lambda (x) (mlistinner bind pure (rest args) (append res (list x)))))]
))

;; Takes a list of monadic operations and binds the results in a new list 
(define (mlist bind pure args) (mlistinner bind pure args empty))


;;;;;;;;;;;;;;;
;; Exercise 5

(define (mapply bind pure f . args)
  (bind (mlist bind pure args) (lambda (xs) (pure (apply f xs)))))

;;;;;;;;;;;;;;;
;; Exercise 6 (MANUALLY GRADED)
#|
The system wouldn't be sound if there was an overflow error because
there could be a scenario where the memory counter gets set back 
to zero which could result in a function being executed using the wrong
reference to a variable. (in the case where one reuses a variable name
inside a function or with a parameter name, and said name exists in the curren
false reference with a different value).
|#
