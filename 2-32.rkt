#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda (x)
                            (cons (car s) x)) rest)))))

;; It's a kind of magic.
;; It works because we are building a binary tree structure starting with a root and two branches with nodes (3) and '(). On every new level we cons the "parent" element on the node to the left branch, and pass along the parent node to the right branch. This way we are continually passing along all new and current subsets to the next level, where we construct all new possible subsets that include (car s).


;;                                     /\
;;                                   -/  \-
;;                                 -/      \
;;                               -/         \-
;;                          (3)-+             |\'()
;;                          --/ |             |  \--
;;                       --/    |             |     \--
;;                    --/       |             |        \--
;;                 --/          |             |           \-
;;              --/             |             |            --
;;           -+/(2 3)           /(3)          |(2)          |\-'()
;;         -/ |               -/|             |\-           |  \--
;;       -/   |             -/  |             |  \-         |     \--
;;     -/     |           -/    |             |    \-       |        \-
;;   -/       |          /      |             |      \      |          \-
;; (1 2 3)  (2 3)      (1 3)   (3)           (1 2)   (2)   (1)          '()
