#lang sicp

(define (cont-frac-iter n d k combiner)
  (define (iter k sum)
    (let ((n-value (n k))
          (d-value (d k)))
      (if (= k 0)
          sum
          (iter (dec k) (/ n-value (combiner d-value sum))))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac-iter (lambda(i)
                    (if (= 1 i)
                        x
                        (* x x)))
                  (lambda(i)
                    (if (= 1 i)
                        1
                        (- (* i 2) 1)))
                  k
                  -))

;;> (tan-cf 100 500)
;; -0.5872139151569293

;; ﻿> (tan 100)
;; -0.587213915156929
