#lang sicp
(#%require (only racket/base random))

(define (timed-fast-prime-test-in-range n times range)
  (cond ((> range 0)
         (fast-prime-test n times (runtime))
         (timed-fast-prime-test-in-range (+ n 1) times (- range 1)))))

(define (fast-prime-test n times start-time)
  (if (fast-prime? n times)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

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

;; Results:
;; 1009 *** 4
;; 1013 *** 3
;; 1019 *** 3

;; 10007 *** 5
;; 10009 *** 4
;; 10037 *** 4

;; 100003 *** 5
;; 100019 *** 5
;; 100043 *** 5

;; 1000003 *** 6
;; 1000033 *** 6
;; 1000037 *** 6

;; With an O(log n) order of growth, I expect the runtime to increase linearly by the length of the number that is tested. So if 1,000 yields a runtime of 3, then 1,000,000 (which is 10^3 greater) should yield a runtime of 6. This is consistent with the results above.
