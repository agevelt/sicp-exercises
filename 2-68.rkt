#lang sicp


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (equal? 'leaf (car object)))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (symbols x)
  (if (leaf? x)
      (list (symbol-leaf x))
      (caddr x)))

(define (weight x)
  (if (leaf? x)
      (weight-leaf x)
      (cadddr x)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit in choose-branch: " bit))))


(define (contains? s symbols)
  (cond ((null? symbols) false)
        ((equal? s (car symbols)) true)
        (else (contains? s (cdr symbols)))))    

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) nil)
        ((contains? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol does not exist in tree: " symbol tree))))




(define (sample-tree)
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


(encode-symbol 'A (sample-tree))
;> (0)
(encode-symbol 'B (sample-tree))
;> (1 0)
(encode-symbol 'C (sample-tree))
;> (1 1 1)
(encode-symbol 'D (sample-tree))
;> (1 1 0)