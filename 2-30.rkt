#lang sicp

(define (square x)
  (* x x))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((list? (car tree)) (cons (square-tree (car tree))
                                  (square-tree (cdr tree))))
        (else (cons (square (car tree))
                    (square-tree (cdr tree))))))

;; ﻿> (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; (mcons
;;  1
;;  (mcons
;;   (mcons 4 (mcons (mcons 9 (mcons 16 '())) (mcons 25 '())))
;;   (mcons (mcons 36 (mcons 49 '())) '())))

(define (square-tree2 tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree2 x)
             (square x)))
       tree))

;; ﻿> (square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; (mcons
;;  1
;;  (mcons
;;   (mcons 4 (mcons (mcons 9 (mcons 16 '())) (mcons 25 '())))
;;   (mcons (mcons 36 (mcons 49 '())) '())))
