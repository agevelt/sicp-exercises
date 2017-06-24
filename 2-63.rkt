#lang racket

(require racket/trace)

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  ;; (trace copy-to-list)
  (copy-to-list tree '()))

;; (trace tree->list-1)
;; (trace tree->list-2)

;; a.
;; tree->list-1 traverses the tree from left to right, while tree->list-2
;; traverses the tree from right to left. The procedures produce the same
;; result for every tree.
(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))
;; ﻿> (tree->list-1 tree1)
;; '(1 3 5 7 9 11)
;; ﻿> (tree->list-2 tree1)
;; '(1 3 5 7 9 11)

(define tree2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))
;; ﻿> (tree->list-1 tree2)
;; '(1 3 5 7 9 11)
;; ﻿> (tree->list-2 tree2)
;; '(1 3 5 7 9 11)

(define tree3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))
;; ﻿> (tree->list-1 tree3)
;; '(1 3 5 7 9 11)
;; > (tree->list-2 tree3)
;; '(1 3 5 7 9 11)



;; b.
;; The order of growth in the number of steps for tree->list-1 is higher, due to
;; the append function being used to flatten and concat subtrees. This is a O(n)
;; operation, which will run on every level of the tree. Assuming a balanced
;; tree, this adds up to O(n*log(n)) number of steps.

;; The tree->list-2 function makes a list by consing the values from right to
;; left, which means it only ever adds one value to the result-list at a time.
;; This adds up to O(n) number of steps.
