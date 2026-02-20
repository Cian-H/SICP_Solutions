(define (bstree-rotate-left tree)
  (if (bstree-empty? tree)
    tree
    (let ((x (bstree-root tree))
          (left-branch (bstree-left-branch tree))
          (y-node (bstree-right-branch tree)))
      (if (bstree-empty? y-node)
        tree
        (let ((y (bstree-root y-node))
              (B (bstree-left-branch y-node))
              (C (bstree-right-branch y-node)))
          (bstree-node y (bstree-node x left-branch B) C))))))

(define (bstree-rotate-right tree)
  (if (bstree-empty? tree)
    tree
    (let ((y (bstree-root tree))
          (x-node (bstree-left-branch tree))
          (right-branch (bstree-right-branch tree)))
      (if (bstree-empty? x-node)
        tree
        (let ((x (bstree-root x-node))
              (A (bstree-left-branch x-node))
              (B (bstree-right-branch x-node)))
          (bstree-node x A (bstree-node y B right-branch)))))))
