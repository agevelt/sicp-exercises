#lang sicp
;; Get element in Pascal's triangle at row r, index i
(define (pascal r i)
  (cond
    ((= i 0) 1)
    ((= i r) 1)
    (else (+ (pascal (dec r) (dec i))
             (pascal (dec r) i)))))
