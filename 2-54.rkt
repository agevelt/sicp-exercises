#lang sicp

(define (equal? a b)
  (cond
    [(and (null? a) (null? b))]
    [(eq? (car a) (car b))
     (equal? (cdr a) (cdr b))]
    [(and (list? a) (list? b))
     (and (equal? (car a) (car b))
          (equal? (cdr a) (cdr b)))]
    [else #f]))
