#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

(define (filter f l)
  (cond ((null? l) nil)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (sum-equal-s? s)
  (lambda (seq)
    (equal? s (accumulate + 0 seq))))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                  (map (lambda (k) (list k j i))
                       (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (find-ordered-triples n s)
  (filter (sum-equal-s? s)
          (unique-triples n)))
