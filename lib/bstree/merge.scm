(define (bstree-merge t1 t2)
  (cond
    ((bstree-empty? t1) t2)
    ((bstree-empty? t2) t1)
    ((not (equal? (bstree-predicate t1) (bstree-predicate t2)))
      (error "Cannot merge two trees with different predicates!"))
    ((equal? (bstree-root t1) (bstree-root t2))
      (bstree-join
        (bstree-root t1)
        (bstree-predicate t1)
        (bstree-merge (bstree-left-branch t1) (bstree-left-branch t2))
        (bstree-merge (bstree-right-branch t1) (bstree-right-branch t2))))
    (else
      (let* ((pred (bstree-predicate t1))
             (sized-trees (if (< (bstree-height t1) (bstree-height t2)) (cons t1 t2) (cons t2 t1)))
             (small (car sized-trees))
             (big (cdr sized-trees))
             (pivot (bstree-root big))
             (big-left (bstree-left-branch big))
             (big-right (bstree-right-branch big))
             (split-result (bstree-split small pivot))
             (small-lt (car split-result))
             (small-gt (cdr split-result)))
        (bstree-join
          pivot
          pred
          (bstree-merge small-lt big-left)
          (bstree-merge small-gt big-right))))))
