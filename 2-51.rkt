#lang sicp
(#%require sicp-pict)

; transform-painter works a little differently in the sicp-pict package than in
; the SICP book. In the book, transform-painter takes a painter as an argument,
; which means we can't reuse the resulting transformation for different painters
; without copy-pasting the code and substituting the painter argument. In the
; sicp-pict package, transform-painter returns a function (painter -> painter).
; This is more general, since it allows transformations to be described
; separately from the application of that transformation to a painter.

(define (below2 painter1 painter2)
  (lambda (frame)
    (let ((down-painter (transform-painter
                         (make-vect 0.0 0.0)
                         (make-vect 1.0 0.0)
                         (make-vect 0.0 0.5)))
          (up-painter (transform-painter
                       (make-vect 0.0 0.5)
                       (make-vect 1.0 0.5)
                       (make-vect 0.0 1.0))))
      ((down-painter painter1) frame)
      ((up-painter painter2) frame))))
