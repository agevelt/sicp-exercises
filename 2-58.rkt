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
  (and (pair? x) (eq? (sign x) '+)))

(define (addend s) (left-arg s))

(define (augend s) (right-arg s))

(define (product? x)
  (and (pair? x) (eq? (sign x) '*)))

(define (multiplier p) (left-arg p))

(define (multiplicand p) (right-arg p))

(define (exponentiation? x)
  (and (pair? x) (eq? (sign x) '**)))

(define (base e) (left-arg e))

(define (exponent e) (right-arg e))

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

(define (parenthesize-expr expr left-arg)
  (if (or (pair? (car expr))
          (pair? (caddr expr))
          (eq? (cdddr expr) '())
          (lowest-precedence? (cadr expr) (cadddr expr)))
      (parenthesize-left-arg expr left-arg)
      (parenthesize-expr (cddr expr) (append left-arg (list (car expr) (cadr expr))))))

(define (parenthesize-left-arg expr arg1)
  (cond ((eq? arg1 '()) expr)
        (else (cons (append arg1 (list (car expr))) (cdr expr)))))

(define (sign expr)
  (cadr (parenthesize-expr expr '())))

(define (left-arg expr)
  (car (parenthesize-expr expr '())))

(define (right-arg expr)
  (let ((expr-p (parenthesize-expr expr '())))
    (cond ((eq? (cdddr expr-p) '()) (caddr expr-p))
          (else (cddr expr-p)))))

(deriv '(x + 3) 'x)

(deriv '(x * y) 'x)

(deriv '((x * y) * (x + 3)) 'x)

;;a. test
(deriv '(x + (3 * (x + (y + 2)))) 'x)

;;b. test
(deriv '(x + 3 * (x + y + 2)) 'x)



