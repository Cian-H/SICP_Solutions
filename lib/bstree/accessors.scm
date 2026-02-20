(define (bstree-tag tree) (car tree))
(define (bstree-tree tree) (cdr tree))
(define (bstree-root tree) (car (bstree-tree tree)))
(define (bstree-branches tree) (cdr (bstree-tree tree)))

(define (bstree-left-branch tree)
  (if (bstree-branches? tree) (cadr (bstree-tree tree)) bstree-empty))

(define (bstree-right-branch tree)
  (if (bstree-branches? tree) (cddr (bstree-tree tree)) bstree-empty))
