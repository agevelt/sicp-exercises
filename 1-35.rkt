#lang sicp

(define tolerance 0.000001)

(define (fixed-point f initial-guess)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try f guess)
    (let ((next (f guess)))
      (if (good-enough? guess next)
          next
          (try f next))))
  (try f initial-guess))

;; > (define (gx x)
;; >   (+ 1 (/ 1 x)))
;; > (fixed-point gx 1.0)
;; 1.618033813400125
