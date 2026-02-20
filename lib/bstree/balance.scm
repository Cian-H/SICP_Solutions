(define (bstree-balance tree)
  (if (bstree-empty? tree)
    tree
    (let* ((left (bstree-left-branch tree))
           (right (bstree-right-branch tree))
           (bf (- (bstree-height left) (bstree-height right))))
      (cond
        ((> bf 1)
          (let ((LL (bstree-height (bstree-left-branch left)))
                (LR (bstree-height (bstree-right-branch left))))
            (if (< LL LR)
              (bstree-rotate-right
                (bstree-node (bstree-root tree) (bstree-rotate-left left) right))
              (bstree-rotate-right tree))))
        ((< bf -1)
          (let ((RL (bstree-height (bstree-left-branch right)))
                (RR (bstree-height (bstree-right-branch right))))
            (if (> RL RR)
              (bstree-rotate-left
                (bstree-node (bstree-root tree) left (bstree-rotate-right right)))
              (bstree-rotate-left tree))))
        (else tree)))))
