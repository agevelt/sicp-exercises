#lang sicp


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))


; a)
; The reason we can't have number and variable in the 'get'-dispatch is because they are primitives.
; The dispatch-table won't have any good way of dispatching based on them since they could be any number
; or any variable. For the other types, we have some identifying tag, like '+', '-', '/', etc.

; b, c)

(define (install-derivation-package)
  (define (derive-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (derive-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  (define (derive-exponentiation exp var)
    (make-product (exponent exp)
                  (make-product (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                                (deriv (base exp) var))))

  (put 'deriv '+ derive-sum)
  (put 'deriv '* derive-product)
  (put 'deriv '** derive-exponentiation)
  )





(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (eq? exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (* b (make-exponentiation b (- e 1))))
        (else (list '** b e))))


(define (addend s) (car s))

(define (augend s) (cadr s))


(define (multiplier p) (car p))

(define (multiplicand p) (cadr p))


(define (base e) (car e))

(define (exponent e) (cadr e))


; d) It seems to me the only thing you would have to do is swap the positions of
; 'derive and '<operator> in:
; (put 'deriv '+ derive-sum)
; (put 'deriv '* derive-product)
; (put 'deriv '** derive-exponentiation)
