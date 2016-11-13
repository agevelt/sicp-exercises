#lang sicp

(define (reverse l)
  (define (reverse-iter l rev)
    (if (null? l)
        rev
        (reverse-iter (cdr l) (cons (car l) rev))))
  (reverse-iter l '()))

;; ﻿> (reverse (list 1 2 3 4 5 9))
;; (mcons 9 (mcons 5 (mcons 4 (mcons 3 (mcons 2 (mcons 1 '()))))))
