#lang sicp

(define (double f)
  (lambda (x)
    (f (f x))))

;; ﻿> (((double (double double)) inc) 5)
;; 21

;; If we convert inc to 1, and double to *2, then it follows that the result is 5+1*2*2*2=21
