#lang sicp

(define dx 0.00001)
(define tolerance 0.000001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (fixed-point f initial-guess)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try f guess)
    (let ((next (f guess)))
      (if (good-enough? guess next)
          next
          (try f next))))
  (try f initial-guess))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define  (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))
