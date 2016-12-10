#lang sicp

(define (reverse l)
  (define (reverse-iter l rev)
    (if (null? l)
        rev
        (reverse-iter (cdr l) (cons (car l) rev))))
  (reverse-iter l '()))
(define (deep-reverse l)
  (define (deep-reverse-iter l r)
    (cond [(null? l) r]
          [(list? (car l)) (deep-reverse-iter (cdr l) (cons (deep-reverse-iter (car l) '()) r))]
          [else (deep-reverse-iter (cdr l) (cons (car l) r))]))
  (deep-reverse-iter l '()))

(define x (list (list 1 2) (list 3 4)))

(reverse x)
;(mcons (mcons 3 (mcons 4 '())) (mcons (mcons 1 (mcons 2 '())) '()))

(deep-reverse x)
;(mcons (mcons 4 (mcons 3 '())) (mcons (mcons 2 (mcons 1 '())) '()))
