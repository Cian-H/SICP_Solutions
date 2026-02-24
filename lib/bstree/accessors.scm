(define (bstree-tag tree) (car tree))
(define (bstree-predicate tree) (cadr tree))
(define (bstree-tree tree) (cddr tree))
(define (bstree-root tree) (car (bstree-tree tree)))
(define (bstree-branches tree) (cdr (bstree-tree tree)))

(define (bstree-left-branch tree)
  (if (bstree-branches? tree)
    (cadr (bstree-tree tree))
    (bstree-make-empty (bstree-predicate tree))))

(define (bstree-right-branch tree)
  (if (bstree-branches? tree)
    (cddr (bstree-tree tree))
    (bstree-make-empty (bstree-predicate tree))))

(define (bstree-lookup key key-getter tree)
  (let ((key-key (key-getter key))
        (pred (bstree-predicate tree)))
    (define (iter tree)
      (if (bstree-empty? tree)
        #f
        (let ((root (bstree-root tree)))
          (cond
            ((equal? key-key (key-getter root)) root)
            ((pred key root) (iter (bstree-left-branch tree)))
            (else (iter (bstree-right-branch tree)))))))
    (iter tree)))
