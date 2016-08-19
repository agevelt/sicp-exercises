#lang sicp

(define (* a b)
  (fast-* a b 0))

(define (fast-* a b n)
  (cond
    ((= b 0) n)
    ((even? b) (fast-* (double a) (halve b) n))
    (else (fast-* a (- b 1) (+ n a)))))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))
