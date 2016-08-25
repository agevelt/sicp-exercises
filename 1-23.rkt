#lang sicp

(define (timed-slow-prime-test n)
  (newline)
  (display n)
  (slow-prime-test n (runtime)))

(define (slow-prime-test n start-time)
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
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (next divisor)
  (if (= 2 divisor)
      3
      (+ divisor 2)))

;Previous runtime:
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

;New runtime:
;1009 *** 6
;1013 *** 6
;1019 *** 6

;10007 *** 8
;10009 *** 8
;10037 *** 9

;100003 *** 16
;100019 *** 16
;100043 *** 17

;1000003 *** 42
;1000033 *** 42
;1000037 *** 41

;6/3=    2,0
;8/7=    1,1428
;16/20=  0,8
;42/65=  0,6461

;To begin with, the extra conditional dominates the savings from the reducing in test steps. After a while, it gradually decreases to about 2/3 of the original function. We halve the number of test steps, but add an additional function calls for every step, which offset some of the savings from the reduction in test steps.
