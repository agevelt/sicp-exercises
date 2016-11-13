#lang sicp

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car t)
  (define (recur a rest)
    (if (odd? rest)
        a
        (recur (+ a 1) (/ rest 2))))
  (recur 0 t))

(define (cdr t)
  (define (recur a rest)
    (if (odd? rest)
        (odd-recur 0 rest)
        (recur (+ a 1) (/ rest 2))))
  (define (odd-recur b rest)
    (if (= 1 rest)
        b
        (odd-recur (+ b 1) (/ rest 3))))
  (recur 0 t))
