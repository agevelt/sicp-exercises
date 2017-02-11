#lang sicp

; In the first version, the queen-cols procedure evaluates queen-cols exactly once, making it a linear recursive process.

; The second version will evaluate queen-cols n times (where n is the board size), making it a exponential recursive process.

; If the solution time of the first version is T, then the second version will clock in at T*n^n.
