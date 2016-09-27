#lang sicp

(define tolerance 0.000001)

(define (fixed-point f initial-guess)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (print-guess x)
    (display x)
    (newline))
  (define (try f guess)
  ;;  (print-guess guess)
    (let ((next (f guess)))
      (if (good-enough? guess next)
          next
          (try f next))))
  (try f initial-guess))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (>= 1 n)
      f
      (compose f (repeated f (- n 1)))))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x))
                 2)))

;; Inserting x and n into:
;; ﻿> (fixed-point ((repeated average-damp n) (lambda (y) (/ x (expt y (- n 1)))))
;;                2.0)

;; Yields terminating combinations:
;; n = 2,3     damps = 1
;; n = 4,5,6,7 damps = 2
;; n = 8       damps = 3

;; log2(7) = 2,807
;; log2(8) = 3

;; Possible n-value:
;; damps = (floor (log2 n))

(define (log2 n)
  (floor (/ (log n)
            (log 2))))

(define (nth-root n x)
  (define (f y) (/ x (expt y (- n 1))))
  (let ((damps (log2 n)))
    (fixed-point ((repeated average-damp damps) f)
                 2.0)))

;; ﻿> (nth-root 154 3)
;; 1.0071593945466397
;; ﻿> (nth-root 123 6)
;; 1.0146742386586478
;; ﻿> (nth-root 1111 6)
;; 1.0016140316640447
;; ﻿> (nth-root 13 9)
;; 1.1841408405165335
