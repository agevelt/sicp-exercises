#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (is-mobile? structure)
  (list? structure))

(define (total-weight mobile)
  (define (branch-rec branch)
    (structure-rec (branch-structure branch)))
  (define (structure-rec structure)
    (if (is-mobile? structure)
        (+ (branch-rec (left-branch structure))
           (branch-rec (right-branch structure)))
        structure))
  (structure-rec mobile))


;; ﻿> (total-weight (make-mobile (make-branch 4 4) (make-branch 4 (make-mobile (make-branch 3 2) (make-branch 8 7)))))
;; 13

(define (is-balanced? structure)
  (define (traverse structure)
    (if (number? structure)
        structure
        (let [(left-struct (traverse (branch-structure (left-branch structure))))
              (right-struct (traverse (branch-structure (right-branch structure))))]
          (if (equal? left-struct right-struct)
              left-struct
              #f))))
  (number? (traverse structure)))

;; ﻿> (is-balanced? (make-mobile (make-branch 4 4) (make-branch 4 (make-mobile (make-branch 3 2) (make-branch 8 7)))))
;; #f

;; ﻿> (is-balanced? (make-mobile (make-branch 4 4) (make-branch 4 3)))
;; #f

;; ﻿> (is-balanced? (make-mobile (make-branch 4 3) (make-branch 4 3)))
;; #t


;; If we were to change the representation of mobiles and branches to use 'cons' instead of 'list' in the constructor, then we would need to change the selectors 'right-branch' and 'branch-structure' to 'cdr' instead of 'cadr'.
