#lang sicp

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (key record)
  record)

(define (lookup given-key tree)
  (cond ((null? tree)
         false)
        ((equal? given-key (key (entry tree)))
         (entry tree))
        ((< given-key (key (entry tree)))
         (lookup given-key (left-branch tree)))
        ((> given-key (key (entry tree)))
         (lookup given-key (right-branch tree)))))

(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

;; ﻿> (lookup 7 tree1)
;; 7
;; ﻿> (lookup 4 tree1)
;; #f
;; ﻿> (lookup 5 tree1)
;; 5
