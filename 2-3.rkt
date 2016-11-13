#lang sicp

;; Points

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Segments

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (x-length s)
  (let ((a (start-segment s))
        (b (end-segment s)))
    (abs (- (x-point a) (x-point b)))))

(define (y-length s)
  (let ((a (start-segment s))
        (b (end-segment s)))
    (abs (- (y-point a) (y-point b)))))

(define (segment-length s)
  (sqrt (+ (expt (x-length s) 2)
           (expt (y-length s) 2))))

(define (horisontal-segment? s)
  (let ((sxl (x-length s))
        (syl (y-length s)))
    (and (> sxl 0) (= syl 0))))

(define (vertical-segment? s)
  (let ((sxl (x-length s))
        (syl (y-length s)))
    (and (> syl 0) (= sxl 0))))

(define (same-startpoint-segments? a b)
  (let ((a-start (start-segment a))
        (b-start (start-segment b)))
    (equal? a-start b-start)))

(define (perpendicular-segments? a b)
  (or (and (vertical-segment? a)
           (horisontal-segment? b))
      (and (vertical-segment? b)
           (horisontal-segment? a))))

;; Rectangles
;; Assumption: all rectangles consist of a vertical and horisontal segments.
;; Both segments must start in the same (x,y) coordinate.

(define (make-rectangle sx sy)
  (if (not (and (same-startpoint-segments? sx sy)
                (perpendicular-segments? sx sy)))
        (display "sx and sy needs to be perpendicular and have an endpoint in common")
        (cons sx sy)))

(define (get-horisontal-segment r)
  (car r))

(define (get-vertical-segment r)
  (cdr r))

(define (get-horisontal-rectangle-length r)
  (segment-length (get-horisontal-segment r)))

(define (get-vertical-rectangle-length r)
  (segment-length (get-vertical-segment r)))

(define (get-rectangle-perimeter r)
  (let ((x-length (get-horisontal-rectangle-length r))
        (y-length (get-vertical-rectangle-length r)))
    (+ (* 2 x-length)
       (* 2 y-length))))

(define (get-rectangle-area r)
  (let ((x-length (get-horisontal-rectangle-length r))
        (y-length (get-vertical-rectangle-length r)))
  (* x-length y-length)))

;; Rectangles v2
;; Assumption: All rectangles consist of three points, an origo, y-point and x-point.

(define (make-rectangle-v2 op xp yp)
  (cons op (cons xp yp)))

(define (get-horisontal-segment-v2 r)
  (make-segment (car r) (cadr r)))

(define (get-vertical-segment-v2 r)
  (make-segment (car r) (cddr r)))
