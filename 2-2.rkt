#lang sicp

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (let* ((a (start-segment s))
         (b (end-segment s))
         (mx (/ (+ (x-point a) (x-point b))
                2))
         (my (/ (+ (y-point a) (y-point b))
                2)))
    (make-segment mx my)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
