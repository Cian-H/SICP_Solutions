(load "lib/functools.scm")

; I think it's asking for a binary search tree here. This will simplify `element-of-set`
; and `union-set` pretty naturally
(define (bstree-insert tree x)
  (if (null? tree)
    (cons x '()) ;; if tree empty return a leaf: (x)
    (let ((root (car tree))
          (branches (cdr tree)))
      (cond
        ((= x root) tree) ;; if x = root just return tree, to avoid duplicates
        ((null? branches)
          (if (< x root)
            (cons root (cons (bstree-insert '() x) '()))
            (cons root (cons '() (bstree-insert '() x)))))
        (else
          (let ((left-branch (car branches))
                (right-branch (cdr branches)))
            (cons root
              (if (< x root)
                (cons (bstree-insert left-branch x) right-branch)
                (cons left-branch (bstree-insert right-branch x))))))))))

; Now, since we're a bstree, these become easy
(define (set . l)
  (apply bstree l))

(define (adjoin-set x set)
  (bstree-insert set x))
