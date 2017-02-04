#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))



(define (matrix-*-vector m v)
  (map (lambda (w) (accumulate + 0
                               (accumulate-n * 1 (list v w))))
       m))

;; ﻿> (matrix-*-vector '((1 2 3 4) (4 5 6 6) (6 7 8 9)) '(1 2 3 4))
;; (mcons 30 (mcons 56 (mcons 80 '())))

(define (transpose mat)
  (accumulate-n cons '() mat))

;; ﻿> (transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
;; (mcons
;;  (mcons 1 (mcons 4 (mcons 6 '())))
;;  (mcons
;;   (mcons 2 (mcons 5 (mcons 7 '())))
;;   (mcons
;;    (mcons 3 (mcons 6 (mcons 8 '())))
;;    (mcons (mcons 4 (mcons 6 (mcons 9 '()))) '()))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (i)
           (map (lambda (j)
                  (accumulate + 0 (accumulate-n * 1 (list i j))))
                cols))
         m)))

;; ﻿> (matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9))'((1 2 3) (4 5 6) (7 8 9)))
;; (mcons
;;  (mcons 30 (mcons 36 (mcons 42 '())))
;;  (mcons
;;   (mcons 66 (mcons 81 (mcons 96 '())))
;;   (mcons (mcons 102 (mcons 126 (mcons 150 '()))) '())))
