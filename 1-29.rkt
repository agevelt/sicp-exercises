#lang sicp

(define (integral f a b n)
  (define h
    (/ ( - b a) n))
  (define (yk k)
    (f (+ a (* h k))))
  (define (simpson-term k)
      (cond
        ((or (= k 0) (= k n)) (yk k))
        ((odd? k) (* 4 (yk k)))
        (else (* 2 (yk k)))))
  (* (/ h 3)
     (sum simpson-term 0 inc n)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) term b))))

(define (cube x)
  (* x x x))

(define (even? n)
  (if (eqv? n 0) #t
      (odd? (- n 1))))

(define (odd? n)
  (if (eqv? n 0) #f
      (even? (- n 1))))
; odd? gives a contract violation on any division
