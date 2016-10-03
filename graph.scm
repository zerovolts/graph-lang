(use srfi-69)
(include "node.scm")

(define (make-index-counter)
  (let ((index 0))
    (lambda ()
      (let ((old-index index))
        (set! index (+ 1 index))
        old-index))))

; remove -- once 'add-node!' is refactored
(define graph-index (make-index-counter))

; incomplete
(define (make-graph . nodes)
  (make-hash-table))

(define (set-node! graph node-index node)
  (hash-table-set! graph
                   node-index
                   node)
  node)

; refactor -- should not rely on global
(define (add-node! graph node)
  (set-node! graph
             (graph-index)
             node))

; unsafe -- does not remove arrows pointing to it
(define (remove-node! graph node-index)
  (let ((old-node (get-node graph node-index)))
    (hash-table-delete! graph node-index)
    old-node))

(define (get-node graph node-index)
  (hash-table-ref graph node-index))

(define (get-nodes graph)
  (hash-table->alist graph))

(define (add-edge! graph in-index out-index)
  (set-node! graph
             in-index
             (add-edge (get-node graph in-index)
                       out-index)))

(define (remove-edge! graph in-index out-index)
  (hash-table-update! graph
                      in-index
                      (lambda (x) (remove-edge x out-index))))



(define (get-children graph node-index)
  (map (lambda (x) (get-node graph x)) (get-edges (get-node graph node-index))))

; returns a list of all node values in a graph
(define (elements graph)
  (map cadr (get-nodes graph)))

; returns a list of all edge pairs in a graph
(define (edge-pairs graph)
  (let ((edges-list (map (lambda (x) (cons (car x) (cddr x))) (get-nodes graph))))
    (flatten (map (lambda (edges) (map (lambda (x) (cons (car edges) x)) (cdr edges))) edges-list))))

(define (node-count graph)
  (hash-table-size graph))

(define (print-graph graph)
  (map (lambda (x) (print (format "node ~s: ~s" (car x) (cdr x)))) (hash-table->alist graph))
  (node-count graph))
