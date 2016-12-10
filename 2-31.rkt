#lang sicp

(define (square x) (* x x))

(define (tree-map f tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map f x)
             (f x)))
       tree))


;; ﻿> (tree-map square (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; (mcons
;;  1
;;  (mcons
;;   (mcons 4 (mcons (mcons 9 (mcons 16 '())) (mcons 25 '())))
;;   (mcons (mcons 36 (mcons 49 '())) '())))
