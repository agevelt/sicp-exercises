#lang racket

; Setup

; Operation table
(define *op-table* (make-hash))

(define (put op type proc)
  (let ((typel (if (pair? type)
                 type
                 `(,type))))
      (hash-set! *op-table* (list op typel) proc)))

(define (get op type)
  (let ((typel (if (pair? type)
                 type
                 `(,type))))
    (hash-ref *op-table* (list op typel) #f)))

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

(define (square x)
  (apply-generic 'mul x x))

; packages
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (apply-generic 'squareroot
                   ('apply-generic 'add
                                   (square (real-part z))
                                   (square (imag-part z)))))
  (define (angle z)
    (apply-generic 'arctan (imag-part z) (real-part z)))
  (define (equ-rectangular z1 z2)
    (let* ((z1r (coerce-to 'real (real-part z1)))
           (z1i (coerce-to 'real (imag-part z1)))
           (z2r (coerce-to 'real (real-part z2)))
           (z2i (coerce-to 'real (imag-part z2))))
      (and ((get 'equ '(real real)) (contents z1r) (contents z2r))
           ((get 'equ '(real real)) (contents z1i) (contents z2i)))))
  (define (make-from-mag-ang r a) 
    (cons (apply-generic 'mul r (apply-generic 'cosine a))
          (apply-generic 'mul r (apply-generic 'sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'equ '(rectangular rectangular) equ-rectangular)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
;gjor disse generic:
; cos sin * 

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (apply-generic 'mul (magnitude z) (apply-generic 'cosine (angle z))))
  (define (imag-part z)
    (apply-generic 'mul (magnitude z) (apply-generic 'sine (angle z))))
  (define (equ-polar z1 z2)
    (let* ((z1m (coerce-to 'real (magnitude z1)))
           (z1a (coerce-to 'real (angle z1)))
           (z2m (coerce-to 'real (magnitude z2)))
           (z2a (coerce-to 'real (angle z2))))
      (and ((get 'equ '(real real)) (contents z1m) (contents z2m))
           ((get 'equ '(real real)) (contents z1a) (contents z2a)))))
  (define (make-from-real-imag x y) 
    (cons (apply-generic 'squareroot (apply-generic 'add (square x) (square y)))
          (apply-generic 'arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ '(polar polar) equ-polar)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))
  ; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (apply-generic 'add (real-part z1) (real-part z2))
                         (apply-generic 'add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
                         (apply-generic 'sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
                       (apply-generic 'add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (apply-generic 'div (magnitude z1) (magnitude z2))
                       (apply-generic 'sub (angle z1) (angle z2))))
  (define (equ-complex z1 z2)
    ((get 'equ `(,(type-tag z1) ,(type-tag z2)))
     (contents z1)
     (contents z2)))
  (define (complex->real z)
    ; To project a complex number, we coerce all underlying representations
    ; to a real number, so we don't have to deal with the underlying
    ; representations in other places.
    ((get 'make 'real) (contents (coerce-to 'real (real-part z)))))
  ; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ '(complex complex) equ-complex)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'project 'complex complex->real)
  'done)
  

(define (install-real-package)
  ; Internal procedures
  (define (tag x)
    (attach-tag 'real x))
  (define (apply-real f x y)
    (tag (apply f `(,(contents x) ,(contents y)))))
  (define (real->complex real)
    ((get 'make-from-real-imag 'complex) (contents real) 0))
  (define (real->rational real)
    (define (decimals r)
      (if (integer? r)
          0
          (+ 1 (decimals (* 10 r)))))
    (let* ((num (contents real))
           (deci (decimals num))
           (r ((get 'make 'rational)
               (* num (expt 10 deci))
               (expt 10 deci))))
      r))
  ; interface to the rest of the system
  (put 'add '(real real)
       (lambda (x y) (apply-real + x y)))
  (put 'sub '(real real)
       (lambda (x y) (apply-real - x y)))
  (put 'mul '(real real)
       (lambda (x y) (apply-real * x y)))
  (put 'div '(real real)
       (lambda (x y) (apply-real / x y)))
  (put 'sine 'real (lambda (x) (tag (sin x))))
  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'squareroot '(real) sqrt)
  (put 'arctan '(real real) atan)
  (put 'equ '(real real)
       equal?)
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'raise 'real
       (lambda (x) (real->complex x))) 
  (put 'project 'real
       (lambda (x) (real->rational x))) 
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
  (define (sine-rat x)
    (sin (/ (numer x) (denom x))))
  (define (cosine-rat x)
    (cos (/ (numer x) (denom x))))
  (define (squareroot-rat x)
    (sqrt (/ (numer x) (denom x))))
  (define (arctan-rat x y)
    (atan (/ (numer x) (denom x))
          (/ (numer y) (denom y))))
  (define (rational->real rat)
    (/ (numer (contents rat)) (denom (contents rat))))
  (define (rational->integer rat)
    (round (/ (numer (contents rat)) (denom (contents rat)))))
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
  (put 'sine 'rational sine-rat)
  (put 'cosine 'rational cosine-rat)
  (put 'squareroot 'rational squareroot-rat)
  (put 'arctan '(rational rational) arctan-rat)
  (put 'make 'rational
       (lambda (n d) (make-rat n d)))
  (put 'raise 'rational
       (lambda (x) (make-real (rational->real x))))
  (put 'project 'rational
       (lambda (x) (make-integer (rational->integer x))))
  'done)


(define (install-integer-package)
  ; Internal procedures
  (define (tag x)
    (attach-tag 'integer x))
  (define (integer->rational int)
    ((get 'make 'rational) (contents int) 1))
  ; interface to the rest of the system
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'equ '(integer integer)
       equal?)
  (put 'sine 'integer
       (lambda (x) (sin x)))
  (put 'cosine 'integer
       (lambda (x) (cos x)))
  (put 'make 'integer
       (lambda (x)
         (if (integer? x)
             (tag x)
             (error "Bad integer -- INSTALL-INTEGER-PACKAGE" x))))
  (put 'raise 'integer
       (lambda (x) (integer->rational x)))
  'done)

; ================================================================

(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-real-package)
(install-rational-package)
(install-integer-package)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

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

(define (coerce-to type arg)
  (or (project-to type arg)
      (raise-to type arg)))
    
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
      (map car
           (filter (lambda (x) ((get 'equ `(,(type-tag arg) ,(type-tag (cdr x))))
                                (contents arg)
                                (contents (cdr x))))
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
(apply-generic 'sub (make-from-mag-ang 4 0) (make-from-real-imag 4 0))
;> '(integer . 0)
(apply-generic 'add (make-from-mag-ang 4 0) (make-from-real-imag 2.5 0))
;> '(rational 13.0 . 2.0)
(apply-generic 'add (make-from-mag-ang (make-rational 2 4) 0) (make-from-real-imag 2.5 0))
;> '(integer . 3.0)

; To be able to use any sort of underlying representation in complex numbers for
; the angles, magnitudes, real and imaginary parts, we need to change a bunch of stuff.

; First we need to generalize the implementations of the polar and the rectangular packages.
; We do this by making sure all their functions use generic methods with apply-generic,
; including sine, cosine, squareroot, arctan

; Since we also want to be able to use the drop functionality from the previous exercise, we
; need to add 'equ functions to complex, rectangular, and polar – that dispatches to the
; 'equ function of the underlying representations. Additionally, we coerce the real part of
; of the complex number to a 'real-type number when we project it, so we don't have to deal
; with f.ex. rational numbers in the real package.
