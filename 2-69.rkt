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


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit in choose-branch: " bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ;symbol
                               (cadr pair)) ;frequency
                    (make-leaf-set (cdr pairs))))))


(define (successive-merge leaf-set)
  (cond ((< (length leaf-set) 2) leaf-set)
        ((= (length leaf-set) 2) (adjoin-set (car leaf-set) (cdr leaf-set)))      
        (else (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
                                            (cddr leaf-set))))))
    

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (test1) '((A 4) (B 2) (C 1) (D 1)))

(make-leaf-set (test1))
; > ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

(generate-huffman-tree (test1))
; > (((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (leaf A 4))
