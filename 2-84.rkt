#lang racket

; Setup

; Operation table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

; type tagging helpers
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'real)  
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)  
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; packages
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x)) 
  (put 'add '(real real)
       (lambda (x y) (+ x y)))
  (put 'sub '(real real)
       (lambda (x y) (- x y)))
  (put 'mul '(real real)
       (lambda (x y) (* x y)))
  (put 'div '(real real)
       (lambda (x y) (/ x y)))
  (put 'make 'real
       (lambda (x) (tag x)))
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
  
  ; coercions
  (define (integer->rational int)
    (make-rat (contents int) 1))
  (define (rational->real rat)
    (/ (numer (contents rat)) (denom (contents rat))))
  
  (put 'raise 'integer (lambda (x) (tag (integer->rational x))))
  (put 'raise 'rational (lambda (x) (attach-tag 'real (rational->real x))))
  'done)


(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x)) 
  (put 'add '(integer integer)
       (lambda (x y) (+ x y)))
  (put 'sub '(integer integer)
       (lambda (x y) (- x y)))
  (put 'mul '(integer integer)
       (lambda (x y) (* x y)))
  (put 'make 'integer
       (lambda (x)
         (if (integer? x)
             (tag x)
             (error "Bad integer -- INSTALL-INTEGER-PACKAGE" x))))
  'done)

; ================================================================

(install-real-package)
(install-rational-package)
(install-integer-package)

(define (make-real n)
  ((get 'make 'real) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-integer n)
  ((get 'make 'integer) n))


(define (raise arg)
  (let* ((type (type-tag arg))
         (coerce (get 'raise type)))
    (if coerce (coerce arg) #f)))

(define (raise-to arg type)
  (if (eq? (type-tag arg) type)
      arg
      (let ((raised-arg (raise arg)))
        (if raised-arg
            (raise-to raised-arg type)
            #f))))

(define (compare-type arg1 arg2)
  ; Returns 0 if the types are equal
  ; Returns -1 if the first type is lower
  ; Returns 1 if the first type is higher
  (cond
    ((eq? (type-tag arg1) (type-tag arg2)) 0)
    ((raise-to arg1 arg2) -1)
    (else 1)))
        
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (try-all-coercions op args)
              (error "No method for these types"
                     (list op type-tags)))))))


;TODO: Gjøre om denne til en lazy/quota liste av argument-lister som innehodler alle permutasjoner av argumentene som er lovlige,
; disse kan siden applyes og den første som fungerer gjør at vi kaster resten av lista.
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
         (or (raise-to arg to-type)
             arg))
       args))


(apply-generic 'add (make-rational 2 1) (make-integer 3)) 
;> '(rational 5 . 1)
(apply-generic 'sub (make-integer 2) (make-rational 2 1)) 
;> '(rational 0 . 1)
(apply-generic 'add (make-rational 2 1) (make-real 2.5)) 
;> 4.5

;Test integer -> rational -> real
(apply-generic 'add (make-integer 1) (make-real 2.5))
;> 3.5
(apply-generic 'div (make-real 2.5) (make-integer 100))
;> 0.025