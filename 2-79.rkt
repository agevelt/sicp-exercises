#lang sicp

(define (install-complex-package)
  ; ... skipping all other procedures and interfaces
  (put 'equ '(complex complex)
       (lambda (x y)
         (and (equal? (magnitude x) (magnitude y))
              (equal? (angle x) (angle y)))))
  )

(define (install-rational-package)
  ; ... skipping all other procedures and interfaces
  (put 'equ '(rational rational)
       (lambda (x y)
         (and (equal? (numer x) (numer y))
              (equal? (denom x) (denom y)))))
  )

(define (install-scheme-number-package)
  ; ... skipping all other procedures and interfaces
  (put 'equ '(scheme-number scheme-number)
       (lambda (x y) (eqv? x y)))
  )

(define (equ? x y) (apply-generic 'equ x y))
