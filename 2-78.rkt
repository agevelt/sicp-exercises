#lang sicp


(define (install-package-scheme-numbers)
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
  )

(define (type-tag datum)
  (cond ((pair? datum) (cdr datum))
        ; This is where the magic happens.
        ((number? datum) 'scheme-number)  
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  ; also here
  (if (eqv? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)  
        (else (error "Bad tagged datum -- CONTENTS" datum))))




