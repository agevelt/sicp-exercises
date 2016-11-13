#lang sicp

(define (square x)
  (* x x))

(define (map f items)
  (if (null? items)
      nil
      (cons (f (car items))
            (map f (cdr items)))))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))
