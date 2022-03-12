#lang racket

(define (rand op . seed)
  (match op
    ('generate (random))
    ('reset (random-seed (car seed)))))

;; (rand 'reset 1)
;; 3-6.rkt> (rand 'generate)
;; 0.5067308904603183
;; 3-6.rkt> (rand 'generate)
;; 0.8476066606357194
;; (rand 'reset 2)
;; 3-6.rkt> (rand 'generate)
;; 0.6479534012671345
;; 3-6.rkt> (rand 'generate)
;; 0.7862747203430958
;; 3-6.rkt> (rand 'reset 1)
;; 3-6.rkt> (rand 'generate)
;; 0.5067308904603183
;; 3-6.rkt> (rand 'generate)
;; 0.8476066606357194
;; (rand 'reset 2)
;; 3-6.rkt> (rand 'generate)
;; 0.6479534012671345
;; 3-6.rkt> (rand 'generate)
;; 0.7862747203430958
