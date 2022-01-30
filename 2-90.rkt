#lang racket

(require quickcheck)
(require rackunit/quickcheck)

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
  (cond ((and (pair? datum) (symbol? (car datum))) (car datum))
        ((number? datum) 'real)  
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)  
        (else (error "Bad tagged datum -- CONTENTS" datum))))


; packages


(define (install-polynomial-term-package)
  (define (tag x) (attach-tag 'term x))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (put 'make-term '(integer Any) (lambda (o c) (tag (make-term o c))))
  (put 'order 'term (lambda (t) (order t)))
  (put 'coeff 'term (lambda (t) (coeff t)))
  (put '=zero? 'term (lambda (t) (=zero? (coeff t))))
  'done)


(define (install-sparse-termlist-package)
  (define (tag-list x) (attach-tag 'sparse-termlist x))
  (define (adjoin-term term term-list) 
    (if (=zero? ((get 'coeff 'term) term))
        term-list
        (cons (attach-tag 'term term) term-list)))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (to-dense-termlist term-list)
    (define (to-dense tl idx-ord)
      (cond [(and (empty-termlist? tl) (> idx-ord 0)) (cons 0 (rest-terms tl (sub1 idx-ord)))]
            [(empty-termlist? tl) tl]
            [else (let ([term-ord (apply-generic 'order (first-term tl))])
                    (cond [(< term-ord idx-ord) (cons 0 (to-dense (rest-terms tl) (sub1 idx-ord)))]
                          [(>= term-ord idx-ord) (cons (apply-generic 'coeff (first-term tl)) (rest-terms tl (sub1 idx-ord)))]))]))
    (to-dense term-list (sub1 (length term-list))))
            
        
  
  ;; interface to the rest of the system
  (put 'first-term 'sparse-termlist (lambda (x) (first-term x)))
  (put 'rest-terms 'sparse-termlist (lambda (x) (tag-list (rest-terms x))))
  (put 'adjoin-term '(term sparse-termlist) (lambda (t l) (tag-list (adjoin-term t l))))
  (put 'empty-termlist? 'sparse-termlist (lambda (l) (empty-termlist? l)))
  (put 'the-empty-termlist 'sparse-termlist (lambda () (tag-list '())))
  ; TODO: Fiks så Any kan bli brukt av hvilken som helst type i apply-generic.
  (put 'to-dense-termlist 'sparse-termlist
       (lambda (l) (attach-tag 'dense-termlist (to-dense-termlist l))))
  'done)
        
(define (install-dense-termlist-package)
  (define (tag-list x) (attach-tag 'dense-termlist x))
  (define (adjoin-term term term-list)
    (let ((term-coeff ((get 'coeff 'term) term))
          (term-order (contents ((get 'order 'term) term))))
      (cond
        [(apply-generic '=zero? term-coeff) term-list]
        [(empty-termlist? term-list) (cons term-coeff (build-list term-order (lambda (x) 0)))]
        [else
         (let ((term-list-coeff ((get 'coeff 'term) (contents (first-term term-list))))
               (term-list-order (contents ((get 'order 'term) (contents (first-term term-list))))))
           (cond [(= term-order (add1 term-list-order)) (cons term-coeff term-list)]
                 [(> term-order term-list-order)
                  (list* term-coeff
                         ; Add 0's in between the order of the term and the order of the first term
                         ; of the term-list.
                         (build-list (sub1 (- term-order term-list-order)) (lambda (x) 0))
                         term-list)]
                 [else (error "Cannot adjoin term to term-list that has a term of the same order"
                              term term-list)]))])))
  (define (first-term term-list)
    ((get 'make-term '(integer Any)) (sub1 (length term-list)) (car term-list)))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (empty-termlist? term-list)
    (cond ((null? term-list) #t)
          ((=zero? (car term-list)) (empty-termlist? (cdr term-list)))
          (else #f)))
  (define (to-sparse-termlist term-list)
    (cond [(empty-termlist? term-list) '()]
          [(=zero? (first-term term-list)) (to-sparse-termlist (rest-terms term-list))]
          [else (cons (first-term term-list) (to-sparse-termlist (rest-terms term-list)))]))
  
  ;; interface to the rest of the system
  (put 'first-term 'dense-termlist (lambda (x) (first-term x)))
  (put 'rest-terms 'dense-termlist (lambda (x) (tag-list (rest-terms x))))
  (put 'adjoin-term '(term dense-termlist) (lambda (t l) (tag-list (adjoin-term t l))))
  (put 'empty-termlist? 'dense-termlist (lambda (l) (empty-termlist? l)))
  (put 'the-empty-termlist 'dense-termlist (lambda () (tag-list '())))
  (put 'to-sparse-termlist 'dense-termlist
       (lambda (l) (attach-tag 'sparse-termlist (to-sparse-termlist l))))
  'done)

(define (install-polynomial-package)
  ;; internal procedures

  ;; representation of poly
  (define (make-poly variable term-list)
    (type-tag term-list)
    (cons variable term-list))
        
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? v)
    (symbol? v))
  (define (same-variable? a b)
    (and (variable? a) (variable? b) (eq? a b)))
  ;; representation of term and term lists
  (define (adjoin-term term term-list)
    (if (eq? term-list '())
        (let ((term1 term)
              (termlist1 term-list))
          ((get 'adjoin-term `(,(type-tag term) sparse-termlist)) term term-list))
        (apply-generic 'adjoin-term term term-list)))
  (define (the-empty-termlist type)
    ((get 'the-empty-termlist type)))
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  (define (rest-terms term-list)
    (apply-generic 'rest-terms term-list))
  (define (empty-termlist? term-list)
    ((get 'empty-termlist? (type-tag term-list)) (contents term-list)))
  (define (make-term order coeff)
    ((get 'make-term '(integer Any)) order coeff))
  (define (order term)
    (apply-generic 'order term))
  (define (coeff term)
    (apply-generic 'coeff term))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let* ((t1 (first-term L1))
                  (t2 (first-term L2))
                  (o1 (contents (order t1)))
                  (o2 (contents (order t2))))
             (cond ((> o1 o2)
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< o1 o2)
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p2) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))

  (define (sub-terms L1 L2)
        (add-terms L1 (negate-termlist L2)))

  (define (negate-poly p)
    (make-poly (variable p) (negate-termlist (term-list p))))

  (define (negate-termlist L)
    (if (empty-termlist? L)
        (the-empty-termlist (type-tag L))
        (let ((t (first-term L)))
          (adjoin-term
           (make-term (order t) (negate (coeff t)))
           (negate-termlist (rest-terms L))))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist (type-tag L1))
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist (type-tag L))
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (add (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (=zero?-poly p)
    (define (is-zero? L)
      (if (empty-termlist? L)
          true
          (and (apply-generic '=zero? (first-term L))
               (is-zero? (rest-terms L)))))
    (is-zero? (term-list p)))
                   
  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-poly p)))
  (put 'negate '(polynomial)
       (lambda (x) (tag (negate-poly x))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done
  )


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (if (=zero? (imag-part z))
        (real-part z)
        (apply-generic 'squareroot
                       (apply-generic 'add
                                      (square (real-part z))
                                      (square (imag-part z))))))
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
           (z2a (coerce-to 'real (angle z2)))
           )
      (and ((get 'equ '(real real)) (contents z1m) (contents z2m))
           ((get 'equ '(real real)) (contents z1a) (contents z2a)))))
  (define (make-from-real-imag x y)
    (cons (cond [(=zero? x) y]
                [(=zero? y) x]
                [else (apply-generic 'squareroot (apply-generic 'add (square x) (square y)))])
          (apply-generic 'arctan y x)))
  (define (equ-polar-and-rectangular zp zr)
    (equ-polar zp (make-from-real-imag (car zr) (cdr zr))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ '(polar polar) equ-polar)
  (put 'equ '(polar rectangular) equ-polar-and-rectangular)
  (put 'equ '(rectangular polar) (lambda (x y) (equ-polar-and-rectangular y x)))
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
  (define (modulo-circle x)
    ; This makes it so pi + pi = 0, instead of 2*pi,
    ; it also assumes it's 0 if it's very close to 0
    ; or 2*pi, just to make the results a little cleaner.
    ; (And also a hacky way of doing it).
    (let* ((xr (coerce-to 'real x))
           (pi2 (* pi 2))
           (res (- xr (* (floor (/ xr pi2)) pi2)))
           (limit-zero 0.000001)) 
      (if (or (< res limit-zero) (< (- res pi2) limit-zero))
          0.0
          res)))
  ; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (apply-generic 'add (real-part z1) (real-part z2))
                         (apply-generic 'add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
                         (apply-generic 'sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
                       (modulo-circle (apply-generic 'add (angle z1) (angle z2)))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (apply-generic 'div (magnitude z1) (magnitude z2))
                       (modulo-circle (apply-generic 'sub (angle z1) (angle z2)))))
  (define (equ-complex z1 z2)
    (apply-generic 'equ z1 z2))
  (define (negate-complex z)
    (make-from-real-imag (negate (real-part z)) (negate (imag-part z))))
    
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
  (put 'negate '(complex)
       (lambda (z) (tag (negate-complex z))))
  (put 'equ '(complex complex) equ-complex)
  (put '=zero? '(complex)
       (lambda (x)
         (and (=zero? (real-part x)) (=zero? (imag-part x)))))
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

  (define (real->complex real)
    ((get 'make-from-real-imag 'complex) (contents real) 0))
  (define (real->rational real)
    (define (decimals r [sum 0] [max-recurse 40])
      (cond [(integer? r) sum]
            [(zero? max-recurse) #f]
            [else (decimals (* 10 r) (add1 sum) (sub1 max-recurse))]))
    (let* ((num (contents real))
           (deci (decimals num)))
      (if (not deci)
          #f
          ((get 'make 'rational) (* num (expt 10 deci)) (expt 10 deci)))))
  (define (equ-real x y)
    ; Polar and rectangular complex numbers have a habit of making the
    ; number inexact, due to several inexact operations, therefore we are a little lenient in
    ; determining the equality here, so that we can use quickcheck to test complex numbers
    ; against the racket number system.

    ; Ideally we would use a representation that minimizes the rounding errors for each operation
    ; like the Kahan method for subtraction, and fused-multiple-add for multiplication.
    ; This will have to do for now though.
    (< (abs (- x y)) 0.01))
  ; interface to the rest of the system
  (put 'add '(real real)
       (lambda (x y) (+ x y)))
  (put 'sub '(real real)
       (lambda (x y) (- x y)))
  (put 'mul '(real real)
       (lambda (x y) (* x y)))
  (put 'div '(real real)
       (lambda (x y) (/ x y)))
  (put 'negate '(real)
       (lambda (x) (- x)))
  (put 'sine 'real (lambda (x) (sin x)))
  (put 'cosine '(real) (lambda (x) (cos x)))
  (put 'squareroot '(real) sqrt)
  (put 'arctan '(real real) (lambda (x y) (atan x y)))
  (put 'equ '(real real) equ-real)
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'make 'real
       (lambda (x) x))
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
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
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
  (put 'negate '(rational)
       (lambda (x) (mul-rat x (contents (make-rat -1 1)))))
  (put 'sine 'rational sine-rat)
  (put 'cosine 'rational cosine-rat)
  (put 'squareroot 'rational squareroot-rat)
  (put 'arctan '(rational rational) arctan-rat)
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
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
  (put 'div '(integer integer)
       (lambda (x y) (/ x y)))
  (put 'equ '(integer integer)
       (lambda (x y) (= x y)))
  (put 'squareroot 'integer sqrt)
  (put 'sine 'integer
       (lambda (x) (sin x)))
  (put 'cosine 'integer
       (lambda (x) (cos x)))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'negate '(integer)
       (lambda (x) (- x)))
  (put 'make 'integer
       (lambda (x)
         (if (integer? x)
             (tag x)
             (error "Bad integer -- INSTALL-INTEGER-PACKAGE" x))))
  (put 'raise 'integer
       (lambda (x) (integer->rational x)))
  'done)

; ================================================================
(install-polynomial-term-package)
(install-sparse-termlist-package)
(install-dense-termlist-package)
(install-polynomial-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-real-package)
(install-rational-package)
(install-integer-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (make-dense-polynomial var terms)
  ((get 'make 'polynomial) var (attach-tag 'dense-termlist terms)))

(define (make-sparse-polynomial var terms)
  (let ((term-list (apply-generic 'to-sparse-termlist (attach-tag 'dense-termlist terms))))
    ((get 'make 'polynomial) var term-list)))

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


(define (=zero? x) (apply-generic '=zero? x))
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (sub x y) (apply-generic 'sub x y))
(define (square x) (apply-generic 'mul x x))
(define (negate x) (apply-generic 'negate x))


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

(define (is-coercible? datum)
  (and
   (not (boolean? datum))
   (or (pair? datum) (number? datum))
   (or (get 'project (type-tag datum)) (get 'raise (type-tag datum)))))
    
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

  (if (is-coercible? arg)
      (let ((l (filtered-dropped-args arg)))
        (car (reverse l)))
      arg))

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
  (define (try-arg from-args)
    (if (empty? from-args)
        (error "No method found for" (list op args))
        (let* ((to-arg (car from-args))
               (to-type (type-tag to-arg))
               (coerced-args (coerce-all to-type args))
               (coerced-type-tags (map type-tag coerced-args))
               (coerced-op (get op coerced-type-tags)))
          (if coerced-op
              (apply coerced-op (map contents coerced-args))
              (try-arg (cdr from-args))))))
  (try-arg args))


(define (coerce-all to-type args)
  (map (lambda (arg)
         (or (raise-to to-type arg)
             arg))
       args))

;quickcheck

(define (racket-equation->sicp-equation e [use-complex? #t])
  (match e
    [(list '+ a b) (add (racket-equation->sicp-equation a use-complex?) (racket-equation->sicp-equation b use-complex?))]
    [(list '* a b) (mul (racket-equation->sicp-equation a use-complex?) (racket-equation->sicp-equation b use-complex?))]
    [(list '/ a b) (div (racket-equation->sicp-equation a use-complex?) (racket-equation->sicp-equation b use-complex?))]
    [(list '- a b) (sub (racket-equation->sicp-equation a use-complex?) (racket-equation->sicp-equation b use-complex?))]
    [a #:when (integer? a) (make-integer a)]
    [a #:when (and (complex? a) use-complex?) (make-from-real-imag (real-part a) (imag-part a))]
    [a #:when (real? a) (make-real a)]
    [a (error "Could not find matching case" a)]))

(define (eval-racket-equation e)
  (if (number? e)
      e
      ((op-to-fn (car e))
       (eval-racket-equation (cadr e))
       (eval-racket-equation (caddr e)))))

(define (op-to-fn op)
  (match op
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]))

(define (generate-equation recurse-limit)
  (bind-generators
   ([op (choose-one-of '(+ - *))]
    [e1 (generate-equation-r recurse-limit)]
    [e2 (generate-equation-r recurse-limit)])
    `(,op ,e1 ,e2)))

(define (generate-equation-r [recurse-limit 1])
  (choose-with-frequencies
   (list
    (cons 1 (choose-integer -10000000 10000000))
    (cons 2 (choose-real -10000000 10000000))
    (cons (if (zero? recurse-limit) 0 5) (generate-equation (sub1 recurse-limit))))))

(define (equation-property use-complex?)
  (property ([e (generate-equation-r)])
            (let ([t1 (racket-equation->sicp-equation (eval-racket-equation e) use-complex?)]
                  [t2 (racket-equation->sicp-equation e use-complex?)])
              (apply-generic 'equ t1 t2))))

(define (conf use-complex?)
  (config 100 10 (lambda (n) (+ 3 (quotient n 2)))
          (lambda (n args)
            (let ([e1 (racket-equation->sicp-equation (cdaar args) use-complex?)]
                  [e2 (racket-equation->sicp-equation (eval-racket-equation (cdaar args)) use-complex?)])
              (println `(test ,n))
              (println `(sicp ,(coerce-to 'real e1)))
              (println `(racket ,(coerce-to 'real e2)))
              (println `(diff ,(abs (- (coerce-to 'real e1) (coerce-to 'real e2)))))
              ))))
; without complex numbers:
(check-property/config (conf #f) (equation-property #f))
; with complex numbers:
(check-property/config (conf #t) (equation-property #t))
; This fails because several operations on complex numbers are inexact and make the result diverge
; a small amount from the non-complex operations. I have tested it and the results are always pretty close
; so there is no bug as far as I can tell, only imprecise results from operations.


; The file 2-90.rkt contains several bug fixes to previous exercises and should be used from exercise 2.77 and up.
; I can't be bothered to backport this, so I will just have to live with several of the previous
; exercises not being particularly correct T.T


; The actual exercise 2-90:
; We split the polynomial package into three layers, where each layer uses functions from the layer below.
; The bottom layer is 'term, which has has functions:
; make-term, order, coeff and =zero?.

; The middle layer is 'dense-termlist or 'sparse-termlist, which has the functions:
; first-term, rest-term- adjoin-term, empty-termlist?, the-empty-termlist

; The top layer is 'polynomial which has the functions:
; add, sub, mul, =zero?, negate, make

; If we want to be able to use both dense-termlist and sparse-termlist interchangably, we need some
; functions to switch from one representation to the other. This is accomplished by to-dense-termlist
; and to-sparse-termlist.


(add (make-dense-polynomial 'x (list (make-real -5.5) 0 0 0 2.5 0   0 (make-from-real-imag -3 1)))
     (make-dense-polynomial 'x (list (make-real -5.5) 0 0 0 8   2.5 0 (make-from-real-imag -3 1))))
;'(polynomial x
;  dense-termlist
;  ((integer . 7) (integer . -11.0))
;  ((integer . 6) (integer . 0))
;  ((integer . 5) (integer . 0))
;  ((integer . 4) (integer . 0))
;  ((integer . 3) (rational 21.0 . 2.0))
;  ((integer . 2) (rational 5.0 . 2.0))
;  ((integer . 1) (integer . 0))
;  ((integer . 0) (complex rectangular (integer . -6) integer . 2)))
(add (make-sparse-polynomial 'x (list (make-real -5.5) 0 0 0 2.5 0   0 (make-from-real-imag -3 1)))
     (make-sparse-polynomial 'x (list (make-real -5.5) 0 0 0 8   2.5 0 (make-from-real-imag -3 1))))
; '(polynomial x
;   sparse-termlist
;   ((integer . 7) (integer . -11.0))
;   ((integer . 3) (rational 21.0 . 2.0))
;   (2 2.5)
;   ((integer . 0) (complex rectangular (integer . -6) integer . 2)))