#lang sicp

(define (f g)
  (g 2))

;; > (f square)
;; 4

;; > (f (lambda (z) (* z (+ z 1))))
;; 6

;; (f f)
;; Expansion:
;; (f (f 2))
;; (f (f (2 2)))
;;        ^ Bad syntax: Integers are not procedures.
