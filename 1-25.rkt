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
  (remainder (fast-expt base exp) m))

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

;; 1009 *** 79
;; 1013 *** 67
;; 1019 *** 43

;; 10007 *** 1876
;; 10009 *** 6856
;; 10037 *** 2494

;; 100003 *** 123085
;; 100019 *** 125426
;; 100043 *** 128339

;; Alyssa's method does not exploit the fact that (x * y) mod m) = ((x mod m) (y mod m) mod m). This allows us to always work with integers close to the size of m. Alyssa's method would mean that we only ever compute the remainder when we have done all the other work, which means we square potentially huge numbers.
