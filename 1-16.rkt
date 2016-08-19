#lang sicp

(define (fast-expt a b)
  (fast-expt-iter a b 1))

(define (fast-expt-iter a b n)
  (cond
    ((= b 0) n)
    ((even? b) (fast-expt-iter (square a)
                               (/ b 2)
                               n))
    (else (fast-expt-iter a
                          (- b 1)
                          (* n a)))))

(define (square x)
  (* x x))
