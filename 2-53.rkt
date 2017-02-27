#lang sicp

;; ﻿> (list 'a 'b 'c)
;; (mcons 'a (mcons 'b (mcons 'c '())))

;; ﻿> (list (list 'george))
;; (mcons (mcons 'george '()) '())

;; ﻿> (cdr '((x1 x2) (y1 y2)))
;; (mcons (mcons 'y1 (mcons 'y2 '())) '())

;; ﻿> (cadr '((x1 x2) (y1 y2)))
;; (mcons 'y1 (mcons 'y2 '()))

;; ﻿> (pair? (car '(a short list)))
;; #f

;; ﻿> (memq 'red '((red shoes) (blue socks)))
;; #f

;; ﻿> (memq 'red '(red shoes blue socks))
;; (mcons 'red (mcons 'shoes (mcons 'blue (mcons 'socks '()))))
