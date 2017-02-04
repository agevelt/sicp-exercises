#lang sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

;; ﻿> (reverse (list 1 2 34 45))
;; (mcons 45 (mcons 34 (mcons 2 (mcons 1 '()))))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; ﻿> (reverse-2 (list 1 2 34 45))
;; (mcons 45 (mcons 34 (mcons 2 (mcons 1 '()))))
