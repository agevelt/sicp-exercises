#lang sicp

(define (element-of-set? x ys)
  (cond ((null? ys) false)
        ((equal? x (car ys)) true)
        (else (element-of-set? x (cdr ys)))))

(define (adjoin-set x ys)
  (if (element-of-set? x ys)
      ys
      (cons x ys)))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (adjoin-set (car set1)
                      (union-set (cdr set1) set2)))))
