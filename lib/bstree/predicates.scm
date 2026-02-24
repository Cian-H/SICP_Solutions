(define (bstree? tree)
  (eq? (bstree-tag tree) 'bstree))

(define (bstree-end? tree)
  (eq? tree bstree-end))

(define (bstree-notree? tree)
  (bstree-end? (bstree-tree tree)))

(define (bstree-empty? tree)
  (and (bstree? tree) (bstree-notree? tree)))

(define (bstree-branches? tree)
  (and
    (not (bstree-empty? tree))
    (not (bstree-end? (bstree-branches tree)))))

(define (bstree-left-branch? tree)
  (and
    (bstree-branches? tree)
    (not (null? (bstree-left-branch tree)))))

(define (bstree-right-branch? tree)
  (and
    (bstree-branches? tree)
    (not (null? (bstree-right-branch tree)))))

(define (bstree-contains? tree x)
  (cond
    ((bstree-empty? tree) #f)
    (else
      (let ((root (bstree-root tree)))
        (cond
          ((equal? x root) #t)
          ((null? (bstree-branches tree)) #f)
          (else
            (if ((bstree-predicate tree) x root)
              (bstree-contains? (bstree-left-branch tree) x)
              (bstree-contains? (bstree-right-branch tree) x))))))))
