(use srfi-1)
(include "graph.scm")

(define (get-dependent-nodes graph)
  (delete-duplicates (flatten (map cddr (get-nodes graph)))))

(define (get-source-nodes graph)
  (lset-difference
   =
   (map car (get-nodes graph))
   (get-dependent-nodes graph)))

(define (get-sink-nodes graph)
  (map car (filter (lambda (x)
                     (eq? '() (get-edges (cdr x))))
                   (get-nodes graph))))

; reverse all arrows
(define (get-dependency-graph graph)
  (let ((dep-graph (alist->hash-table (map (lambda (x) (cons (car x) (list (cadr x)))) (get-nodes graph))))
        (connections (map (lambda (x) (list (car x) (cddr x))) (get-nodes graph))))
    (map (lambda (x) (map (lambda (y) (add-edge! dep-graph y (car x))) (cadr x))) connections)
    dep-graph))

(define (get-reducible-subgraphs graph)
  (let ((connections (map (lambda (x) (list (car x) (cddr x))) (get-nodes (get-dependency-graph graph))))
        (source-nodes (get-source-nodes graph)))
    (filter (lambda (x) (and (not (null-list? (cadr x))) (lset<= = (cadr x) source-nodes))) connections)))

(define (eval-nodes sink sources)
  (apply (get-value sink) (map get-value sources)))

(define (reduce-nodes! graph sink-index source-indices)
  (let ((sink
         (get-node graph sink-index))
        (sources
         (map (lambda (x)
                (let ((new-node (remove-edge! graph x sink-index)))
                  (if (empty? new-node) (remove-node! graph x) new-node)))
              source-indices)))
    (set-node!
     graph
     sink-index
     (set-value sink (eval-nodes sink sources)))))

(define (reduce-graph! graph)
  (map (lambda (x) (reduce-nodes! graph (car x) (cadr x))) (get-reducible-subgraphs graph))
  (if (null? (get-reducible-subgraphs graph)) graph (reduce-graph! graph)))

;
(define (eval-graph! graph)
  (elements (reduce-graph! graph)))
