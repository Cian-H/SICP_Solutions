(define *bstree-heights* (racket:make-weak-hasheq))

(define (bstree-set-height tree h)
  (racket:hash-set! *bstree-heights* tree h))

(define (bstree-height tree)
  (cond
    ((bstree-empty? tree) 0)
    ((racket:hash-has-key? *bstree-heights* tree)
      (racket:hash-ref *bstree-heights* tree))
    (else
      (let ((h (+ 1 (max (bstree-height (bstree-left-branch tree))
                     (bstree-height (bstree-right-branch tree))))))
        (bstree-set-height tree h)
        h))))
