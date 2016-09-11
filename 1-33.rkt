#lang sicp

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (if (filter a)
                                     (term a)
                                     null-value)
                                 result))))
  (iter a null-value))

(define (relatively-prime? b)
  (lambda (a) (= (gcd a b) 1)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (id x) x)

;; ﻿> (filtered-accumulate + prime? 0 square a inc b)

;; ﻿> (filtered-accumulate * (relatively-prime? 10) 1 id 1 inc 10)
;; 189
