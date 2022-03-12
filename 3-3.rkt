#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (cond ((not (eq? pass password)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"))))
  dispatch)

(define a (make-account 100 'abc))
((a 'abc 'deposit) 100)
((a 'abc 'withdraw) 1)
((a 'xyz 'withdraw) 1)


(define b (make-account 100 'xyz))
((b 'xyz 'withdraw) 1000)
((b 'abc 'withdraw) 1)
