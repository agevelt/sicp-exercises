#lang sicp

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; Each sine call reduces the angle by 3.0, which means we get a logarithmic growth in number of steps.
;; The process is not tail-recursive, which yields a linear growth in space.
