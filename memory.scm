; virtual memory table

; get, set, add

; +]

(define (make-index-counter)
  (let ((index 0))
    (lambda ()
      (let ((old-index index))
        (set! index (+ 1 index))
        old-index))))

; (memory) -> #(v1 v2 v3 v4)
; (memory 3) -> "value at index 3"
; (memory 3 "hello")
; (memory add "hello")
(define (make-virtual-memory-table)
  (let ((memory (make-vector 20 'empty))
        (cursor (make-index-counter)))
    (lambda args
      (case (length args)
            ((0) memory)
            ((1) (vector-ref memory (car args)))
            ((2) (vector-set! memory
                              (if (eq? 'add (car args))
                                  (cursor)
                                  (car args))
                              (cadr args)))))))
