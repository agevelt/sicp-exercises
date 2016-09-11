#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product id 1 inc n))

(define (pi n) ;; TODO
  (* 4
     (/ (* n (product double-square 4 inc (- n 1)))
        (product double-square 3 inc n))))

(define (double-square n)
  (* 2 (* n n)))

(define (id a) a)
