#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))


;; ﻿> ((compose (lambda(x) (* x x)) inc) 6)
;; 49
