#lang sicp

(define l1 '(1 3 (5 7) 9))
(define l2 '((7)))
(define l3 '(1 (2 (3 (4 (5 (6 7)))))))

(display (car (cdaddr l1)))
(newline)
(display (caar l2))
(newline)
(display (cadadr (cadadr (cadadr l3))))
