#lang sicp
(#%require sicp-pict)

(define (flip-horiz2 painter)
  (let ((flip (transform-painter
               (make-vect 1.0 0.0)
               (make-vect 0.0 0.0)
               (make-vect 1.0 1.0))))
    (flip painter)))

(define (rotate-180 painter)
  (let ((rotate (transform-painter
                 (make-vect 1.0 1.0)
                 (make-vect 0.0 1.0)
                 (make-vect 1.0 0.0))))
    (rotate painter)))

(define (rotate-270 painter)
  (let ((rotate (transform-painter
                 (make-vect 1.0 0.0)
                 (make-vect 1.0 1.0)
                 (make-vect 0.0 0.0))))
    (rotate painter)))
