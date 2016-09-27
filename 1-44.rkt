#lang sicp

(define dx 0.00001)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (>= 1 n)
      f
      (compose f (repeated f (- n 1)))))

(define (average-3 x y z)
  (/ (+ x y z)
     3))

(define (smooth f)
  (lambda (x)
    (average-3 (f (- x dx))
               (f x)
               (f (+ x dx)))))

(define (smooth-n-fold f n)
  (lambda (x)
    (((repeated smooth n) f) x)))
