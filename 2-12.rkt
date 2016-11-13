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

(define (div-interval x y)
  (if (and (>= (upper-bound y) 0)
           (<= (lower-bound y) 0))
      (display "Cannot divide with an interval spanning 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width2 i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-interval (- c w)
                   (+ c w))))

(define (percent i)
  (/ (* 100 (width2 i))
     (center i)))

(define (get-multiplied-percent a b)
  (+ (percent a) (percent b)))

(define (test-percent)
  (let ((regular-percent (percent (mul-interval (make-center-percent 20 4)
                                                (make-center-percent 10 8))))
        (simple-percent (get-multiplied-percent (make-center-percent 20 4)
                                                (make-center-percent 10 8))))
    (display "regular: ")
    (display regular-percent)
    (display "\nsimple: ")
    (display simple-percent)))
