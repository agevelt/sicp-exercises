#lang sicp

(define  (make-interval x y)
  (cons x y))

(define (lower-bound a)
  (car a))

(define (upper-bound a)
  (cdr a))

(define (width a)
  (/ (- (upper-bound a) (lower-bound a))
     2))

(define (add-interval a b)
  (make-interval (+ (lower-bound a) (lower-bound b))
                 (+ (upper-bound a) (upper-bound b))))

(define (mul-interval a b)
  (let ((p1 (* (lower-bound a) (lower-bound b)))
        (p2 (* (lower-bound a) (upper-bound b)))
        (p3 (* (upper-bound a) (lower-bound b)))
        (p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (should-be-true a b a2 b2)
  (= (width (add-interval a b))
     (width (add-interval a2 b2))))

; If the width of the product is a function of the width of the intervals, then we should get the same width as long as the intervals we supply have the same width. a-b = a2-b2.
;﻿> (should-be-true (make-interval 2 2) (make-interval 3 5)
;                  (make-interval 5 5) (make-interval 9 11))
;#t

(define (should-be-false a b a2 b2)
  (= (width (mul-interval a b))
     (width (mul-interval a2 b2))))

;﻿> (should-be-false (make-interval 2 2) (make-interval 3 5)
;                   (make-interval 5 5) (make-interval 9 11))
;#f
