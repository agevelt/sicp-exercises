#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus a b)
  (lambda (f) (lambda (x) ((lambda (y) ((a f) y)) ((b f) x)))))

(define (print-number n) ((n (lambda (f) (+ 1 f))) 0))

;﻿> (print-number (plus two two))
;4
;﻿> (print-number (plus two (plus two two)))
;6
;﻿> (print-number (plus (plus one two) (plus two two)))
;7
