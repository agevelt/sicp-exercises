#lang sicp

(define (square x)
  (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;﻿> (square-list '(1 3 5 9))
;(mcons 81 (mcons 25 (mcons 9 (mcons 1 '()))))


(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;﻿> (square-list2 '(1 3 5 9))
;(mcons (mcons (mcons (mcons '() 1) 9) 25) 81)

; Usually, a recursive process on a list will cons down the data-structure. This means that the resulting list will mirror the traversal of the original list.

; When we use an iterative process, we bring the current answer with us down through the list. Either the resulting list is reversed, or the data structure it is comprised of is reversed.
