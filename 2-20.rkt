#lang sicp

(define (same-parity x . l)
  (define (parity-iter l)
    (if (null? l)
        nil
        (let ((first (even? x))
              (current (even? (car l))))
          (cond ((equal? first current) (cons (car l) (parity-iter (cdr l))))
                (else (parity-iter (cdr l)))))))
  (cons x (parity-iter l)))

(define (filter f l)
  (cond ((null? l) nil)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

(define (same-parity2 x . l)
  (filter (lambda (y) (equal? (even? x) (even? y)))
          (cons x l)))
