#lang racket

(define code (quote
    (define p (point x y z)
    (first p))
))

(rest (third code))

(symbol? (second code))
(second code)

; hw solution v
(equal? (first code) 'define)
(symbol? (second code))
(third code)
