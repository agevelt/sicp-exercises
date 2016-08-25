#lang sicp

(define (start-test n)
  (test-all-carmichael-numbers n 3))

(define (test-all-carmichael-numbers n i)
  (cond
    ((>= i n) true)
    (else (carmichael-test n i (runtime))
          (test-all-carmichael-numbers n (+ i 1)))))

(define (carmichael-test n a start-time)
  (if (expmod-test n a)
      (report-prime n a (- (runtime) start-time))))

(define (report-prime n a elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display a)
  (display " *** ")
  (display elapsed-time))

(define (expmod-test n a)
    (= (expmod a n n) a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square n)
  (* n n))
