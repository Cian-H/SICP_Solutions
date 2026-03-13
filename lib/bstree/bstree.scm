(define bstree-tag-symbol 'bstree)
(define bstree-end 'bstree-end)
(define bstree-null-predicate null?)
(define bstree-empty (cons bstree-tag-symbol (cons bstree-null-predicate bstree-end)))

(define (bstree-node x pred left right)
  (let ((h (+ 1 (max (bstree-height left) (bstree-height right)))))
    (cons 'bstree (cons pred (cons x (cons h (cons left right)))))))
