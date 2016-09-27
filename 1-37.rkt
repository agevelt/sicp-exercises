#lang sicp

(define (cont-frac n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (/ (n k) (+ (d k) (cont-frac n d (dec k))))))

;; ﻿> (cont-frac (lamb;; da (i) 1.0) (lambda (i) 1.0) 10)
;; ;; 0.6179775280898876
;; ;; ﻿> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
;; ;; 0.6180555555555556

;; Truncating the answer after 11 terms yields an answer accurate to 4 decimal points

(define (cont-frac-iter n d k)
  (define (iter n d k sum)
    (if (= k 0)
        sum
        (iter n d (dec k) (/ (n k) (+ (d k) sum)))))
  (iter n d k 0))

;; ﻿> (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)
;; 0.6179775280898876
;; ﻿> (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)
;; 0.6180555555555556
