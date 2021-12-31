#lang sicp


; a) If we install a coercion from type scheme-number->scheme-number, then we would enter
; an infinite loop of calling apply-generic, since we keep coercing to the same type, which
; we have already tested and found no operation for.

; b) apply-generic works correctly as it is, except in the case where we add coercions between
; the same types, since that can lead to an infinite loop.

; c)
; old version:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method found for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; version that doesnt try type-coercion for the same types:
(define (apply-generic1 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                ; New check here
                (if (equal? type1 type2)
                    (error "No method found for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic1 op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic1 op a1 (t2->t1 a2)))
                            (else
                             (error "No method found for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))
