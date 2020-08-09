#lang sicp

;; a.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (- (exponent exp) 1))))
        (else
         (error "unknown expression type -- DERIV" exp))))


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (eq? exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (lowest-prec-sign x) '+)))

(define (addend s) (lowest-prec-arg1 s))

(define (augend s) (lowest-prec-arg2 s))

(define (product? x)
  (and (pair? x) (eq? (lowest-prec-sign x) '*)))

(define (multiplier p) (lowest-prec-arg1 p))

(define (multiplicand p) (lowest-prec-arg2 p))

(define (exponentiation? x)
  (and (pair? x) (eq? (lowest-prec-sign x) '**)))

(define (base e) (lowest-prec-arg1 e))

(define (exponent e) (lowest-prec-arg2 e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (* b (make-exponentiation b (- e 1))))
        (else (list b '** e))))


(define (lowest-precedence? current next)
  (cond ((and (eq? current '+) (eq? next '*)) #t)
        ((and (eq? current '+) (eq? next '**)) #t)
        ((and (eq? current '*) (eq? next '**)) #t)
        (else #f)))

(define (lowest-prec-sign expr)
  (cond ((or (pair? (car expr)) (pair? (caddr expr))) (cadr expr))
        ((eq? (cdddr expr) '()) (cadr expr))
        ((lowest-precedence? (cadr expr) (cadddr expr)) (cadr expr))
        (else (lowest-prec-sign (cddr expr)))))

(define (lowest-prec-arg1 expr)
  (cond ((or (pair? (car expr)) (pair? (caddr expr))) (car expr))
        ((eq? (cdddr expr) '()) (car expr))
        ((lowest-precedence? (cadr expr) (cadddr expr)) (car expr))
        (else (list (car expr) (cadr expr) (lowest-prec-arg1 (cddr expr))))))

(define (lowest-prec-arg2 expr)
  (cond ((pair? (caddr expr)) (caddr expr))
        ((pair? (car expr)) (cddr expr))
        ((eq? (cdddr expr) '()) (caddr expr))
        ((lowest-precedence? (cadr expr) (cadddr expr)) (cddr expr))
        (else (lowest-prec-arg2 (cddr expr)))))

(deriv '(x + 3) 'x)

(deriv '(x * y) 'x)

(deriv '((x * y) * (x + 3)) 'x)

;;a. test
(deriv '(x + (3 * (x + (y + 2)))) 'x)



;;b. test
(deriv '(x + 3 * (x + y + 2)) 'x)



