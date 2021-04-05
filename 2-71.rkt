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
  (cond ((leaf? tree) '())
        ((contains? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol does not exist in tree: " symbol tree))))

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




(define alphabet5 '((a 1) (b 2) (c 4) (d 8) (e 16)))
(define tree5 (generate-huffman-tree alphabet5))
; The length of the most frequent symbol (e) is:
(length (encode '(e) tree5))
;> 1
; And the least frequent (a and b):
(length (encode '(a) tree5))
;> 4
(length (encode '(b) tree5))
;> 4


(define alphabet10 '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)))
(define tree10 (generate-huffman-tree alphabet10))
; The length of the most frequent symbol (j) is:
(length (encode '(j) tree10))
;> 1
; And the least frequent (a and b):
(length (encode '(a) tree10))
;> 9
(length (encode '(b) tree10))
;> 9

; In a general tree of size n where the symbol frequency is 1,2,4...n^(n-1), the most frequent symbol
; will always have a 1-bit length encoding. While the least frequent symbol will have a (n-1)-bit length
; encoding.

; This is because the sum of the two least frequent symbols will always be less than the third least
; frequent symbol. Example: (1 + 2 = 3) < 4, (3 + 4 = 7) < 8, (7 + 8 = 15) < 16, etc.

; This leads to a list-like tree-shape, with one long spine, and one leaf node at every branch, with the
; exception of the last branch, which has two leaves (the branch with leaf nodes 'a' and 'b'). Since 'a'
; is in the position where the spine normally is, we can encode both the least frequent symbols with the
; same amount of bits.