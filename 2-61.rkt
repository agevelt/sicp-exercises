#lang sicp

(define (element-of-set? x set1)
  (cond ((null? set1) false)
        ((= x (car set1)) true)
        ((< x (car set1)) false)
        (else (element-of-set? x (cdr set1)))))

(define (adjoin-set x set1)
  (cond ((null? set1) x)
        ((= x (car set1)) set1)
        ((< x (car set1)) (cons x set1))
        (else (cons (car set1)
                    (adjoin-set x (cdr set1))))))

;; Compared to the unordered version of adjoin-set, this version approximately
;; halves the number of steps, since we only search the set for numbers <= x.
