#lang racket

(define (make-monitored f)
  (let ((calls 0))
    (lambda (x . xs)
      (cond [(eq? 'how-many-calls? x) calls]
            [(eq? 'reset-count x) (set! calls 0)]
            [else (begin (set! calls (add1 calls))
                         (apply f (cons x xs)))]))))

(define a (make-monitored sqrt))

(a 'how-many-calls?)
(a 10)
(a 'how-many-calls?)
(a 20)
(a 'how-many-calls?)
(a 'reset-count)
(a 'how-many-calls?)
(a 5)
(a 'how-many-calls?)