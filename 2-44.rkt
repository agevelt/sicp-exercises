#lang sicp

(#%require sicp-pict)

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
                 (beside top-left corner))))))
