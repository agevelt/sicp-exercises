#lang sicp

(define (square x) (* x x))

(define (apply-generic op args) (args op))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-AND" op))))
  dispatch)

           
(apply-generic 'angle (make-from-mag-ang 10 30))
;> 30
(apply-generic 'magnitude (make-from-mag-ang 10 30))
;> 10


; pi/4 = 45 degrees => real-part = imag-part
(apply-generic 'imag-part (make-from-mag-ang 1 (/ 3.141592654 4)))
;> 0.7071067811140326
(apply-generic 'real-part (make-from-mag-ang 1 (/ 3.141592654 4)))
;> 0.7071067811140326

; pi/6 = 30 degrees => 30-60-90 triangle => magnitude = 2 * imag-part
(apply-generic 'imag-part (make-from-mag-ang 1 (/ 3.141592654 6)))
;> 0.5000000000592083