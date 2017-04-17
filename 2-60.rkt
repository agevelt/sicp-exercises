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


;; The regular intersection-set definition assumes that both sets contain at
;; most one of each element. If an element in set1 matches against an element in
;; set2, then we remove the element from set2 before the next recursion. The
;; number of steps is the same: O(n*m).

;; (define (intersection-set set1 set2)
;;   (cond ((or (null? set1) (null? set2)) '())
;;         ((element-of-set? (car set1) set2)
;;          (cons (car set1)
;;                (intersection-set (cdr set1) set2)))
;;         (else (intersection-set (cdr set1) set2))))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 (remove-first-elem-in-set (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

(define (remove-first-elem-in-set x ys)
  (cond ((null? ys) '())
        ((equal? x (car ys)) (cdr ys))
        (else (cons (car ys)
                    (remove-first-elem-in-set x (cdr ys))))))

;; In practice, by allowing duplicates we make insertion operations constant.
;; This comes at the cost of having larger sets when doing comparison
;; operations. Since we don't remove duplicates during each operation, we will
;; have a monotonically increasing set size for every set-operation we perform,
;; with the exception of intersection-set, which still functions as advertised.

;; This representation might be useful in cases where you add to, and take the
;; union of sets much more often than you do intersection or check if an element
;; exists in the set.
