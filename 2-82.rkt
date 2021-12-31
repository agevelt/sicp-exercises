#lang racket

; Setup

; operation table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

; coercion table
(define *coercion-table* (make-hash))

(define (put-coercion from-type to-type proc)
  (hash-set! *coercion-table* (list from-type to-type) proc))

(define (get-coercion from-type to-type)
  (hash-ref *coercion-table* (list from-type to-type) #f))


; type tagging helpers
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)  
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (cond ((eqv? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)  
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; packages
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x)) 
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  'done)


(define (install-rational-package)
  ; Internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rat x y)
    (and (equal? (numer x) (numer y))
         (equal? (denom x) (denom y))))
  (define (scheme-number->rational int)
    (make-rat (contents int) 1))
  (define (rational->scheme-number rat)
    (/ (numer (contents rat)) (denom (contents rat))))

  ; Coercions
  ; I have made the assumption that both integers and floats are captured by the 'scheme-number type.
  (put-coercion 'scheme-number 'rational (lambda (x) (tag (scheme-number->rational x))))
  (put-coercion 'rational 'scheme-number (lambda (x) (tag (rational->scheme-number x))))
  
  ; Interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y)))) 
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ '(rational rational)
       (lambda (x y) (equ-rat x y)))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; Installation
(install-scheme-number-package)
(install-rational-package)



; =======================================================================================
; Assignment starts here.


; If there are more than two arguments, we can do something like the code below to
; coerce all arguments to each of the argument types in turn.


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (try-all-coercions op args)
              (error "No method for these types"
                     (list op type-tags)))))))

(define (try-all-coercions op args)
  (define (try-arg to-args)
    (if (empty? to-args)
        (error "No method found for" (list op args))
        (let* ((to-arg (car to-args))
               (to-type (type-tag to-arg))
               (coerced-args (coerce-all to-type args))
               (coerced-type-tags (map type-tag coerced-args))
               (coerced-op (get op coerced-type-tags)))
          (if coerced-op
              (apply coerced-op (map contents coerced-args))
              (try-arg (cdr to-args))))))
  (try-arg args))


(define (coerce-all to-type args)
  (map (lambda (arg)
         (let* ((from-type (type-tag arg))
                (content (contents arg))
                (coerce (get-coercion from-type to-type)))               
           (cond ((equal? from-type to-type) arg)
                 (coerce (coerce arg))
                 (else arg))))
       args))


(apply-generic 'add (make-rational 2 1) 3) 
;> '(rational 5 . 1)
(apply-generic 'sub 2 (make-rational 2 1)) 
;> '(rational 0 . 1)
(apply-generic 'add 2.5 (make-rational 2 1)) 
;> '(rational 9.0 . 2.0)

; This will only handle coercions where the to-type is the same as one of the
; types in the argument list. We would for example not be able to convert something like
; (+ int int) to (+ float float), since we don't have float types in the argument list –
; we only have ints.

; We would also not be able to handle the case where there is a valid coercion path that
; has intermediate values with this approach. So if there is a valid 'add for '(float float),
; and we have '(rational integer), and the coercions integer->rational and rational->float, then
; we would be able to convert the rational to a float, but not the integer, since we don't handle
; transitive coercions.

; Another case we wouldn't be able to catch is where there is a valid operation:
; (op float int float)
; And we want to get there from:
; (op int float float)
; In this case we would not be able to convert to (op float int float), because we only
; coerce to operations where all the arguments are the same:
; (op float float float) and (op int int int).

; To handle all coercions, we would need to try to get the operation for every possible
; legal permutation of coerced arguments. This would ensure that if there was an operation
; that could possibly work with our arguments, we would try it.

; An additional issue is how we should choose if there are several legal operations to
; choose between when we try coercing the arguments. Perhaps we pick the operation that is
; "closest" in the type-hierarchy to the original argument types?

