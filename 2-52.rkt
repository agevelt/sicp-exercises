#lang sicp
(#%require sicp-pict)


(define (wave)
  (define (ms x1 y1 x2 y2)
    (make-segment (make-vect x1 y1) (make-vect x2 y2)))
  (segments->painter
   (list (ms 0.3 0.0 0.35 0.5)
         (ms 0.35 0.5 0.3 0.55)
         (ms 0.3 0.55 0.2 0.4)
         (ms 0.2 0.4 0.0 0.6)
         (ms 0.4 0.0 0.5 0.2)
         (ms 0.5 0.2 0.6 0.0)
         (ms 0.7 0.0 0.6 0.4)
         (ms 0.6 0.4 1.0 0.2)
         (ms 1.0 0.3 0.7 0.65)
         (ms 0.7 0.65 0.6 0.65)
         (ms 0.6 0.65 0.63 0.8)
         (ms 0.63 0.8 0.6 1.0)
         (ms 0.4 1.0 0.37 0.8)
         (ms 0.37 0.8 0.4 0.65)
         (ms 0.4 0.65 0.3 0.65)
         (ms 0.3 0.65 0.2 0.6)
         (ms 0.2 0.6 0.0 0.8)
         (ms 0.5 0.3 0.4 0.5) ;; diamonds are better than smiles, right?
         (ms 0.5 0.3 0.6 0.5)
         (ms 0.6 0.5 0.5 0.7)
         (ms 0.4 0.5 0.5 0.7))))

;﻿> (paint (wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside top top))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (below (beside painter bottom-right)
                 (beside top-left bottom-right))))))

;﻿> (paint (corner-split (wave) 3))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half)
             half))))

;﻿> (paint (square-limit (wave) 3))
