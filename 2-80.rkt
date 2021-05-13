#lang sicp

(define (install-complex-package)
  ; ... skipping all other procedures and interfaces
  (put '=zero? '(complex)
       (lambda (x)
         (and (eqv? (real-part x) 0) (eqv? (imag-part x) 0))))
  )

(define (install-rational-package)
  ; ... skipping all other procedures and interfaces
  (put '=zero? '(rational)
       (lambda (x)
         (or (eqv? (numer x) 0) (eqv? (denom x) 0))))
  )

(define (install-scheme-number-package)
  ; ... skipping all other procedures and interfaces
  (put '=zero? '(scheme-number)
       (lambda (x) (eqv? x 0)))
  )

(define (=zero? x) (apply-generic '=zero? x))

