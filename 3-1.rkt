#lang racket

(define (make-accumulator sum)
  (lambda (to-add)
    (begin (set! sum (+ sum to-add))
           sum)))

(define A (make-accumulator 20))
(define B (make-accumulator 20))

(A 10)
;> 30
(A 70)
;> 100
(B 20)
;> 40
(B -40)
;> 0