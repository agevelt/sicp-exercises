#lang sicp

(#%require sicp-pict)

(define (split op1 op2)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  (lambda (painter n) (splitter painter n)))

(define up-split (split below beside))

(define right-split (split beside below))
