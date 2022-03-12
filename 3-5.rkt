#lang racket


(define (random-int)
  (floor (random-in-range 0 10000000)))

(define (monte-carlo trails experiment)
  (define (iter trails-remaining trails-passed)
    (if (= trails-remaining 0)
        (/ trails-passed trails)
        (if (experiment)
            (iter (- trails-remaining 1) (+ trails-passed 1))
            (iter (- trails-remaining 1) trails-passed))))
  (iter trails 0))

(define (cesaro-test)
  (= (gcd (random-int) (random-int)) 1))

(define (estimate-pi trails)
  (sqrt (/ 6 (monte-carlo trails cesaro-test))))


;; 3-5.rkt> (estimate-pi 20)
;; 2.7386127875258306
;; 3-5.rkt> (estimate-pi 200)
;; 3.2587526795614106
;; 3-5.rkt> (estimate-pi 2000)
;; 3.141404312187716
;; 3-5.rkt> (estimate-pi 20000)
;; 3.1446385396603898
;; 3-5.rkt> (estimate-pi 200000)
;; 3.141494734801315
;; 3-5.rkt> (estimate-pi 2000000)
;; 3.142448522852879


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (estimate-integral trails pred x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)
     (monte-carlo trails pred)))

(define (circle-test radius)
  (lambda ()
    (let ((x (random-in-range 0 (* radius 2)))
          (y (random-in-range 0 (* radius 2))))
      (<= (+ (expt (- x radius) 2) (expt (- y radius) 2))
          (expt radius 2)))))

(define (estimate-pi-via-circle-integration trails radius)
  (let* ((coord1 0)
         (coord2 (* radius 2))
         (pred (circle-test radius)))
    ; We need this to convert from a fraction to a real number,
    ; so we can easily see if the result looks like pi.
    (exact->inexact
     (/ (estimate-integral trails pred coord1 coord2 coord1 coord2)
        (expt radius 2)))))


;; 3-5.rkt> (estimate-pi-via-circle-integration 100 5 7 3)
;; 3.24
;; 3-5.rkt> (estimate-pi-via-circle-integration 1000 5 7 3)
;; 3.164
;; 3-5.rkt> (estimate-pi-via-circle-integration 10000 5 7 3)
;; 3.1504
;; 3-5.rkt> (estimate-pi-via-circle-integration 100000 5 7 3)
;; 3.14268
;; 3-5.rkt> (estimate-pi-via-circle-integration 1000000 5 7 3)
;; 3.138016
;; 3-5.rkt> (estimate-pi-via-circle-integration 10000000 5 7 3)
;; 3.141766
