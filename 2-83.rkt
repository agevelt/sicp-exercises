#lang racket

; We assume that coercions in this table only moves *up* the numerical tower
; So integer->rational->real
(define *coercion-table* (make-hash))

(define (put-coercion from-type proc)
  (hash-set! *coercion-table* from-type proc))

(define (get-coercion from-type)
  (hash-ref *coercion-table* from-type #f))

(define (raise arg)
  (let* ((type (type-tag arg))
         (coerce (get-coercion type)))
    (if coerce (coerce arg) #f)))