(define (bstree-height tree)
  (if (bstree-empty? tree)
    0
    (cadr (bstree-tree tree))))
