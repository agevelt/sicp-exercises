#lang sicp

(define (for-each f l)
  (cond ((null? l) nil)
        (else  (f (car l))
               (for-each f (cdr l)))))

;; ﻿> (for-each (lambda (x) (display x) (newline)) '(1 3 5 9))
;; 1
;; 3
;; 5
;; 9
;; '()
