#lang sicp

(define (make-rat n d)
  (let* ((neg? (not (equal? (< n 0) (< d 0))))
        (g (gcd n d))
        (n (if neg? (- (abs n)) (abs n)))
        (d (abs d)))
    (cons (/ n g) (/ d g))))


;; ﻿> (display (car (make-rat 9 3)))
;; 3
;; ﻿> (display (cdr (make-rat 9 3)))
;; 1
