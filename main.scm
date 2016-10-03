(include "evaluate.scm")

(define expr (make-graph))
(add-node! expr (make-node + '()))
(add-node! expr (make-node * '(0)))
(add-node! expr (make-node * '(0)))
(add-node! expr (make-node + '(1 2)))
(add-node! expr (make-node 2 '(1)))
(add-node! expr (make-node 5 '(2)))
(add-node! expr (make-node 7 '(3)))
(add-node! expr (make-node 12 '(3)))

(print (eval-graph! expr))

; (!(+ 7 12) !(* 2) !(* 5))
