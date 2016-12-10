#lang sicp

(define (concat x y)
  (cond [(null? x) y]
        [(list? x) (cons (car x) (concat (cdr x) y))]
        [else (cons x y)]))

(define (map f x)
  (if (null? x)
      '()
      (cons (f (car x))
            (map f (cdr x)))))

(define (reduce n f l)
  (if (null? l)
      n
      (f (car l) (reduce n f (cdr l)))))

(define (concat-map f x)
  (reduce '()
          concat
          (map f x)))

(define (fringe tree)
  (concat-map (lambda (x)
         (cond [(null? x) '()]
               [(list? x) (fringe x)]
               [else x]))
       tree))

;; ﻿> (fringe (list (list 1 2) (list 3 (list 4 5 6) 4)))
;; (mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 (mcons 6 (mcons 4 '())))))))
