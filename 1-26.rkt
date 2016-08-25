#lang sicp

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; The problem with this function is that (expmod base (/ exp 2) m) is called twice. This means that the function generates an exponential recursive process, instead of a linear recursive process. Since the original function halves the size of the input for every recursion, this new change counteracts that, and we are left with an O(n) runtime.
