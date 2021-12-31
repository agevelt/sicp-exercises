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
  (define (apply-real f x y)
    (tag (apply f `(,(contents x) ,(contents y)))))
  (put 'add '(real real)
       (lambda (x y) (apply-real + x y)))
  (put 'sub '(real real)
       (lambda (x y) (apply-real - x y)))
  (put 'mul '(real real)
       (lambda (x y) (apply-real * x y)))
  (put 'div '(real real)
       (lambda (x y) (apply-real / x y)))
  (put 'equ '(real real)
       equal?)
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)


(define (install-rational-package)
  ; Internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (tag (cons (/ n g) (/ d g)))))
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
       (lambda (x y) (add-rat x y)))
  (put 'sub '(rational rational)
       (lambda (x y) (sub-rat x y)))
  (put 'mul '(rational rational)
       (lambda (x y) (mul-rat x y))) 
  (put 'div '(rational rational)
       (lambda (x y) (div-rat x y)))
  (put 'equ '(rational rational)
       (lambda (x y) (equ-rat x y)))
  (put 'make 'rational
       (lambda (n d) (make-rat n d)))
  
  ; raise
  (define (integer->rational int)
    (make-rat (contents int) 1))
  
  (define (rational->real rat)
    (/ (numer (contents rat)) (denom (contents rat))))
  
  (put 'raise 'integer (lambda (x) (integer->rational x)))
  (put 'raise 'rational (lambda (x) (make-real (rational->real x))))


  ; project
  (define (real->rational real)
    (define (decimals r)
      (if (integer? r)
          0
          (+ 1 (decimals (* 10 r)))))
    (let* ((num (contents real))
           (deci (decimals num))
           (r (make-rat
               (* num (expt 10 deci))
               (expt 10 deci))))
      r))
  
  (define (rational->integer rat)
    (round (/ (numer (contents rat)) (denom (contents rat)))))

  (put 'project 'real (lambda (x) (real->rational x))) 
  (put 'project 'rational (lambda (x) (make-integer (rational->integer x))))

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
  (put 'equ '(integer integer)
       equal?)
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

(define (raise-to type arg)
  (if (eq? (type-tag arg) type)
      arg
      (let ((raised-arg (raise arg)))
        (if raised-arg
            (raise-to type raised-arg)
            #f))))

(define (project arg)
  (let* ((type (type-tag arg))
         (coerce (get 'project type)))
    (if coerce (coerce arg) #f)))

(define (project-to type arg)
  (if (eq? (type-tag arg) type)
      arg
      (let ((projected-arg (project arg)))
        (if projected-arg
            (project-to type projected-arg)
            #f))))



    
(define (drop arg)
  (define (projected-arg-list arg)
    (let ((projected-arg (project arg)))
      (cons arg (if projected-arg
                    (projected-arg-list projected-arg)
                    '()))))

  (define (get-all-dropped-args arg)
    ; Returns a list of tuples of (projected-arg, re-raised-arg)
    ; If the re-raised-arg is the same as arg, the arg can be dropped to the
    ; projected-arg without losing information.
    (map (lambda (a)
           (cons a (raise-to (type-tag arg) a)))
         (projected-arg-list arg)))

  (define (filtered-dropped-args arg)
    ; returns a list of valid dropped args, the last in the list is the
    ; deepest in the numerical tower we can go without losing information.
      (map (lambda (x) (car x))
           (filter (lambda (x) (equal? arg (cdr x)))
                   (get-all-dropped-args arg))))

  (let ((l (filtered-dropped-args arg)))
    (car (reverse l))))

(define (compare-type arg1 arg2)
  ; Returns 0 if the types are equal
  ; Returns -1 if the first type is lower
  ; Returns 1 if the first type is higher
  (cond
    ((eq? (type-tag arg1) (type-tag arg2)) 0)
    ((raise-to (type-tag arg2) arg1) -1)
    (else 1)))
        
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (drop
       (if proc
           (apply proc (map contents args))
           (if (> (length args) 1)
               (try-all-coercions op args)
               (error "No method for these types"
                      (list op type-tags))))))))


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
         (or (raise-to to-type arg)
             arg))
       args))


(apply-generic 'add (make-rational 2 1) (make-integer 3)) 
;> '(integer . 5)
(apply-generic 'sub (make-integer 2) (make-rational 2 1)) 
;> '(integer . 0)
(apply-generic 'add (make-rational 2 1) (make-real 2.5)) 
;> '(rational 9.0 . 2.0)
(apply-generic 'add (make-integer 1) (make-real 2.5))
;> '(rational 7.0 . 2.0)
(apply-generic 'div (make-real 2.5) (make-integer 100))
;> '(rational 1.0 . 40.0)
(apply-generic 'add (make-rational 2 3) (make-rational 4 3)) 
;> '(integer . 2)
(drop (make-real 0.5))
;> '(rational 1.0 . 2.0)
(apply-generic 'add (make-real 3.0) (make-rational 5 1))
;> '(integer . 8.0)