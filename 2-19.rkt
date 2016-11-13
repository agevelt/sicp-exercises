#lang sicp

(define us-coins (list 50 25 10 5 1))

(define us-coins-scrambled (list 1 25 5 10 50))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)

(define except-first-denomination cdr)

(define no-more? null?)

;; The order of coin-values does not impact the answer. This is because + is associative and the procedure recurses over all possible combinations of coin-values >= amount.

;; ﻿> (cc 100 us-coins)
;; 292
;; ﻿> (cc 100 us-coins-scrambled)
;; 292
