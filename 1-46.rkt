#lang racket

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define tolerance 0.00001)

(define (average x y) (/ (+ x y) 2))

(define (sqrt-iter x first-guess)
  (define (good-enough? guess)
    (< (abs (- x (expt guess 2))) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) first-guess))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (let ((next (f guess)))
      (< (abs (- guess next)) tolerance)))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))
