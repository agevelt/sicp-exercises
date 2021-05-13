#lang sicp

(define (install-rectangular-package)
  ;... skipping all other procedures and interfaces, see p. 182
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (put 'magnitude '(rectangular) magnitude)
  )

(define (install-complex-package)
  ;... skipping all other procedures and interfaces, see p. 191
  (put 'magnitude '(complex) magnitude)
  )

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (magnitude z) (apply-generic 'magnitude z))


(magnitude (make-complex-from-real-imag 3 4))
; This works because we have two layers of type tags and the magnitude we install in the complex
; package refers back to the generic magnitude procedure.

; The generic magnitude calls apply-generic, which strips the 'complex tag and uses it to get the
; magnitude procedure from the 'complex package. Since the complex version of magnitude refers back
; to the generic magnitude version, we do a second call to apply-generic, but this time with the
; 'complex tag stripped, so we are only left with the 'rectangular tag. This tag is used to choose the
; rectangular version of the magnitude procedure. The rectangular version then does the actual
; operation, and returns the magnitude up through the call stack.