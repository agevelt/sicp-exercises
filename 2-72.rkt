#lang sicp

; When the tree is structured as in 2.71, the tree will look almost identical to a list structure,
; which has one long spine with one leaf node at each branch with the exception of the final branch,
; which has two leaf nodes.

; The depth of the tree grows linearly with the number of symbols in the alphabet. Similarly, the
; number of symbols we have to scan at each branch level is (n - branch level). So for an alphabet
; n = (a 1, b 2, c 4, d 8, e 16), we have to scan through n symbols at the first branch, n-1 symbols
; at the second branch level, etc.

; Since our encoding function always checks the left node first, and our tree encodes the structure
; described above as a long spine extending to the right, the steps needed to encode the most
; frequent symbol will always be constant = O(1).

; The least frequent symbol must traverse through all n levels of the tree, and in addition check
; the symbol list at each level. This amounts to sum(n..1) = O(n/2*(n+1)) = O(n^2).
