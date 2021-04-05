#lang sicp

;; Since we only need to see if there exists an element x in a set,
;; element-of-set? is the same whether we allow duplicates or not.

(define (element-of-set? x ys)
  (cond ((null? ys) false)
        ((equal? x (car ys)) true)
        (else (element-of-set? x (cdr ys)))))


;; When we allow duplicates, adjoin-set does not need to check if an element is
;; present in the set. The number of steps decreases from O(n) to O(1).

;; (define (adjoin-set x ys)
;;   (if (element-of-set? x ys)
;;       ys
;;       (cons x ys)))
(define (adjoin-set x ys)
  (cons x ys))


;; The union-set definition does not change, since it relies on adjoin-set to do
;; the dirty work. The number of steps decreases from O(n*m) to O(n).

;; (define (union-set set1 set2)
;;   (cond ((null? set1) set2)
;;         (else (adjoin-set (car set1)
;;                           (union-set (cdr set1) set2)))))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (adjoin-set (car set1)
                          (union-set (cdr set1) set2)))))


;; The intersection-set definition does not change if we don't care about limiting
;; the amount of duplicates in the set.

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;; If we care about the intersection-set having the 'right' number of elements,
;; then we can remove the element from set2 before we recurse. The number of steps
;; then increases from O(n*m) to O(n*m*m).

(define (intersection-set2 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set2 (cdr set1)
                                 (remove-first-elem-in-set (car set1) set2))))
        (else (intersection-set2 (cdr set1) set2))))

(define (remove-first-elem-in-set x ys)
  (cond ((null? ys) '())
        ((equal? x (car ys)) (cdr ys))
        (else (cons (car ys)
                    (remove-first-elem-in-set x (cdr ys))))))

;; ------
;; By allowing duplicates, we make the insertion operations faster. This
;; comes at the cost of having larger sets when doing comparison operations.

;; This representation might be useful in cases where you add to and take the
;; union of sets much more often than you take the intersection or check if an
;; element exists in the set.
