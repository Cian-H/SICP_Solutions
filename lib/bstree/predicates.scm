(define (bstree? tree)
  (eq? (bstree-tag tree) 'bstree))

(define (bstree-end? tree)
  (eq? tree bstree-end))

(define (bstree-empty? tree)
  (eq? tree bstree-empty))

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
          ((= x root) #t)
          ((null? (bstree-branches tree)) #f)
          (else
            (if (< x root)
              (bstree-contains? (bstree-left-branch tree) x)
              (bstree-contains? (bstree-right-branch tree) x))))))))
