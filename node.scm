; supplies a few *functional* node primitives -- nodes are treated as immutable
; does not deal with dereferencing node indices
; key: "x -> y" means "x evaluates to y"

; dependency graph of these functions
; -----------------------------------
; make-node
; get-value
; get-edges
; set-value -> make-node get-edges
; set-edges -> make-node get-value
; add-edge -> set-edges get-edges
; remove-edge -> set-edges get-edges
; delete-edge -> set-edges get-edges

; a node is a value and a list of node indices
;> (make-node "hello" '(0 1 2)) -> '("hello" 0 1 2)
(define (make-node value edges)
  (cons value edges))

;> (get-value '("hello" 0 1 2)) -> "hello"
(define (get-value node)
  (car node))

;> (get-edges '("hello 0 1 2")) -> '(0 1 2)
(define (get-edges node)
  (cdr node))

(define (out-degree node)
  (length (get-edges node)))

; sink?
(define (empty? node)
  (null? (get-edges node)))

;> (set-value '("hello" 0 1 2) "hey") -> '("hey" 0 1 2)
(define (set-value node value)
  (make-node
   value
   (get-edges node)))

;> (set-edges '("hello" 0 1 2) '(3 4 5)) -> '("hello" 3 4 5)
(define (set-edges node edges)
  (make-node
   (get-value node)
   edges))

;> (add-edge '("hello" 0 1 2) 6) -> '("hello" 6 0 1 2)
(define (add-edge node node-index)
  (set-edges
   node
   (cons node-index
         (get-edges node))))

; returns a new node identical to node, but with all edges equal to 'node-index' removed
;> (remove-node '("hello" 9 4 7 4) 4) -> '("hello" 9 7)
(define (remove-edge node node-index)
  (set-edges
    node
    (delete node-index
            (get-edges node))))

; returns a new node identical to node, but with the edge 'node-index' removed (does not remove duplicates)
; no built-in function to remove first occuredges an element.
;> (remove-node '("hello" 9 4 7 4) 4) -> '("hello" 9 7 4)
(define (delete-edge node node-index)
  0)
