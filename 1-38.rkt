#lang sicp

(define (cont-frac-iter n d k)
  (define (iter n d k sum)
    (if (= k 0)
        sum
        (iter n d (dec k) (/ (n k) (+ (d k) sum)))))
  (iter n d k 0))

(define (euler-approximation k)
  (define (ni i) 1)
  (define (di i)
    (if (= 0 (modulo (+ i 1) 3))
        (- i
           (- (/ (+ i 1) 3)
              1))
        1))
  (+ 2 (cont-frac-iter ni di k)))
