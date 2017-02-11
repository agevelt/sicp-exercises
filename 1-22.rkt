#lang sicp

(#%provide prime?)

(define (timed-prime-test-in-range a b)
  (cond ((> a b) #f)
        (else  (if (odd? a)
                      (timed-prime-test a))
               (timed-prime-test-in-range (+ a 1) b))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

;Runtime:
;1009 *** 3
;1013 *** 2
;1019 *** 3

;10007 *** 7
;10009 *** 7
;10037 *** 7

;100003 *** 20
;100019 *** 20
;100043 *** 20

;1000003 *** 65
;1000033 *** 65
;1000037 *** 65

;Order of growth:
;﻿> (* (sqrt 10) 2)
;6.324555320336759
;> (* (sqrt 10) 3)
;9.486832980505138
;﻿> (* (sqrt 10) 7)
;22.135943621178658
;﻿> (* (sqrt 10) 20)
;63.24555320336759

;The timing seems to match close to the order of growth O(sqrt(n)). Some deviance can be seen at the smallest primes,
;this is most likely due to the low granularity of the timing data.
