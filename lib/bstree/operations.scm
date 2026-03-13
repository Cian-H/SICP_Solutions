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
  (let ((pred (bstree-predicate tree)))
    (if (bstree-empty? tree)
      (bstree-node x pred (bstree-make-empty pred) (bstree-make-empty pred))
      (let ((root (bstree-root tree))
            (left (bstree-left-branch tree))
            (right (bstree-right-branch tree)))
        (cond
          ((equal? x root) tree)
          ((pred x root) (bstree-join root pred (bstree-insert left x) right))
          (else (bstree-join root pred left (bstree-insert right x))))))))

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
      (bstree-make-empty (bstree-predicate tree))
      (let ((root (bstree-root t))
            (pred (bstree-predicate t))
            (left (bstree-left-branch t))
            (right (bstree-right-branch t)))
        (cond
          ((equal? x root) (bstree-make-empty pred))
          ((pred x root) (bstree-node root pred (prune-raw left) right))
          (else (bstree-node root pred left (prune-raw right)))))))
  (list->bstree (bstree-predicate tree) (bstree->list (prune-raw tree))))

(define (bstree-update tree x key-getter)
  (let ((key-key (key-getter x))
        (pred (bstree-predicate tree)))
    (define (update-iter t)
      (if (bstree-empty? t)
        (error "Key not found in tree!")
        (let ((root (bstree-root t))
              (left (bstree-left-branch t))
              (right (bstree-right-branch t)))
          (cond
            ((equal? key-key (key-getter root))
              (bstree-node x pred left right))
            ((pred x root)
              (bstree-node root pred (update-iter left) right))
            (else
              (bstree-node root pred left (update-iter right)))))))
    (update-iter tree)))
