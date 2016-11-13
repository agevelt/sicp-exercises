#lang sicp

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

;; ﻿> (last-pair (list 1 2 4 5 29))
;; 29
