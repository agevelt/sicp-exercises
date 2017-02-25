#lang sicp
(#%require sicp-pict)

; transform-painter works a little differently in sicp-pict package than in the
; SICP book. The difference being that in sicp-pict, a transform-painter returns
; a function (painter -> painter). This is more general, since it allows
; transformations to be described separately from the application of said
; transformation to a painter. In the book, painter is a parameter of
; transform-painter, which means we can't reuse the given transformation for
; different painters without copy-pasting the definition of the transformation
; and substituting the painter argument.

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
