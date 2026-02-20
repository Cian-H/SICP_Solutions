(define (bstree-min tree)
  (if (bstree-left-branch? tree)
    (bstree-min (bstree-left-branch tree))
    (bstree-root tree)))

(define (bstree-max tree)
  (if (bstree-right-branch? tree)
    (bstree-max (bstree-right-branch tree))
    (bstree-root tree)))

(define (bstree-size tree)
  (if (bstree-empty? tree)
    0
    (+ 1 (bstree-size (bstree-left-branch tree)) (bstree-size (bstree-right-branch tree)))))

(define (bstree-insert tree x)
  (if (bstree-empty? tree)
    (bstree-node x bstree-empty bstree-empty)
    (let ((root (bstree-root tree))
          (left (bstree-left-branch tree))
          (right (bstree-right-branch tree)))
      (cond
        ((= x root) tree)
        ((< x root) (bstree-join root (bstree-insert left x) right))
        (else (bstree-join root left (bstree-insert right x)))))))

(define (bstree-append tree l)
  (if (null? l)
    tree
    (bstree-append (bstree-insert tree (car l)) (cdr l))))

(define (bstree-remove tree x)
  (if (bstree-contains? tree x)
    (let ((parts (bstree-split tree x)))
      (bstree-merge (car parts) (cdr parts)))
    (error (string-append "Value " (number->string x) " not found in tree!"))))

(define (bstree-prune tree x)
  (define (prune-raw t)
    (if (bstree-empty? t)
      bstree-empty
      (let ((root (bstree-root t))
            (left (bstree-left-branch t))
            (right (bstree-right-branch t)))
        (cond
          ((= x root) bstree-empty)
          ((< x root) (bstree-node root (prune-raw left) right))
          (else (bstree-node root left (prune-raw right)))))))
  (list->bstree (bstree->list (prune-raw tree))))
