#lang racket
(require racket/pretty)

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

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
                      remaining-elts))))))))

(define (union-list list1 list2)
  (cond ((null? list2) list1)
        ((null? list1) list2)
        (else  (let ((x1 (car list1))
                     (x2 (car list2)))
                 (cond ((= x1 x2)
                        (cons x1 (union-list (cdr list1) (cdr list2))))
                       ((< x1 x2)
                        (cons x1 (union-list (cdr list1) list2)))
                       ((> x1 x2)
                        (cons x2 (union-list list1 (cdr list2)))))))))

(define (union-set tree1 tree2)
  (let ((list1 (tree->list tree1))
        (list2 (tree->list tree2)))
    (let ((ordered-list (union-list list1 list2)))
      (list->tree ordered-list))))


(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define tree2
  (make-tree 4
             (make-tree 0 '() '())
             (make-tree 8
                        (make-tree 5 '() '())
                        (make-tree 14
                                   (make-tree 11 '() '())
                                   (make-tree 18 '() '())))))

(pretty-print (union-set tree1 tree2))
;; ﻿>
;; '(7
;;   (3 (0 ()
;;         (1 () ()))
;;      (4 ()
;;         (5 () ())))
;;   (11 (8 ()
;;          (9 () ()))
;;       (14 ()
;;           (18 () ()))))

(define (intersection-list list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (let ((x1 (car list1)) (x2 (car list2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list (cdr list1) (cdr list2))))
              ((< x1 x2)
               (intersection-list (cdr list1) list2))
              ((> x1 x2)
               (intersection-list list1 (cdr list2)))))))

(define (intersection-set tree1 tree2)
  (let ((list1 (tree->list tree1))
        (list2 (tree->list tree2)))
    (let ((ordered-list (intersection-list list1 list2)))
      (list->tree ordered-list))))

(pretty-print (intersection-set tree1 tree2))
;; ﻿>
;; '(5 ()
;;     (11 ()
;;         ()))
