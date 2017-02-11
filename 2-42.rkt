#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

(define (filter f l)
  (cond ((null? l) nil)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (make-queen col row)
  (cons col row))

(define (queen-col queen)
  (car queen))

(define (queen-row queen)
  (cdr queen))

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-queen k new-row)
        rest-of-queens))

(define (safe? k positions)
  (define (same-row? current other)
    (equal? (queen-row current)
            (queen-row other)))

  (define (same-diagonal? current other)
    (let ((col-diff (abs (- (queen-col current)
                            (queen-col other))))
          (row-diff (abs (- (queen-row current)
                            (queen-row other)))))
      (equal? col-diff row-diff)))

  (define (safe-pair? current)
    (lambda (other)
      (and (not (same-row? current other))
           (not (same-diagonal? current other)))))

  (if (= k 1)
      true
      (let ((current-queen (car positions))
            (rest (cdr positions)))
        (accumulate (lambda (b1 b2) (and b1 b2))
                    true
                    (map (safe-pair? current-queen)
                         rest)))))
