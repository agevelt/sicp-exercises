#lang sicp

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts)))))))

;; a.
;; partial-tree has two parameters:
;; 1. An ordered list.
;; 2. How many elements should be used to construct a tree.

;; The return value is a cons consisting of:
;; 1. The constructed tree.
;; 2. The unused elements from the ordered list.

;; First, partial-tree divides the second parameter n into left-size =
;; (floor(n/2)). That number in addition to the ordered list are then used as
;; the inputs for the left-hand partial-tree. The returned list of unused
;; elements from this function call is then used as to create the entry node,
;; and also as input to the right subtree, along with the second parameter
;; right-size = ((left-size+1) - n). When this function call returns, then the
;; original function combines the subtrees and the entry
;; node into a new tree and returns this tree along with any unused elements.
 
;;            +---+
;;            | 5 |
;;            +/--+-
;;        /----     \----
;;     /--               \--
;; +---+                 +---+
;; | 1 |                 | 9 |
;; +---+                 +---+
;;     \---              -/  \---
;;         \-           /        \-
;;       +---+      +---+      +----+
;;       | 3 |      | 7 |      | 11 |
;;       +---+      +---+      +----+


;; b. The function splits the computation in two, with the combined size of n-1.
;; This patterns continues recursively until the size is 0, with every
;; computation combining an entry node with two subtrees. The result is a O(n)
;; growth in the number of steps.
