#lang racket


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





(define pairs '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))

(define tree (generate-huffman-tree pairs))

(define message ; #lang sicp doesn't have 'string-split', so this file is in #lang racket
  (map string->symbol (string-split "get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom")))

(define encoded (encode message tree))

(define decoded (decode encoded tree))


message
;> '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom)
decoded
;> '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom)
(length message)
;> 36
(length decoded)
;> 36
(length encoded)
;> 84

; If we were to use a fixed-length code for the eight-symbol alphabet, we would need log2(8) = 3 bits per symbol.
; The size of the fixed-length encoding would be 3 * 36 = 108.
; The huffman encoding is (84/108) = 0,777.. = 77.7% of the size of the fixed-length encoding. 


