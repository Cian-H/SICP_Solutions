(#%require (prefix racket: racket/base))
(load "lib/functools.scm")
(load "lib/sort.scm")

; Originally, i thought exercise 2.62 was asking for a btree, but i later realised that exercise
; wanted a binary search instead. So, i've kept my binary search tree implementation from 2.62
; and i'm re-using it here instead of the implementation in SICP.
(define bstree-height-cache (racket:make-weak-hasheq))

(define (bstree-node val left right)
  (let ((node (if (and (null? left) (null? right))
                  (cons val '())
                  (cons val (cons left right)))))
    (racket:hash-set! bstree-height-cache node (+ 1 (max (bstree-height left) (bstree-height right))))
    node))

(define (bstree-root tree) (car tree))
(define (bstree-branches tree) (cdr tree))

(define (bstree-left-branch tree)
  (if (null? (bstree-branches tree)) '() (cadr tree)))

(define (bstree-right-branch tree)
  (if (null? (bstree-branches tree)) '() (cddr tree)))

(define (bstree-height tree)
  (cond
    ((null? tree) 0)
    ((racket:hash-has-key? bstree-height-cache tree)
     (racket:hash-ref bstree-height-cache tree))
    (else
     (let ((h (+ 1 (max (bstree-height (bstree-left-branch tree))
                        (bstree-height (bstree-right-branch tree))))))
       (racket:hash-set! bstree-height-cache tree h)
       h))))

(define (bstree-partial elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (bstree-partial elts left-size))
             (left-tree (car left-result))
             (non-left-elts (bstree-branches left-result))
             (this-entry (car non-left-elts))
             (right-size (- n (+ left-size 1)))
             (right-result (bstree-partial (bstree-branches non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (bstree-branches right-result)))
        (cons (bstree-node this-entry left-tree right-tree) remaining-elts))))

; Add rotations and balancing, otherwise managing a bstree will become a pain in the ass
(define (bstree-rotate-left tree)
  (if (null? tree)
      '()
      (let ((x (bstree-root tree))
            (left-branch (bstree-left-branch tree))
            (y-node (bstree-right-branch tree)))
        (if (null? y-node)
            tree
            (let ((y (bstree-root y-node))
                  (B (bstree-left-branch y-node))
                  (C (bstree-right-branch y-node)))
              (bstree-node y (bstree-node x left-branch B) C))))))

(define (bstree-rotate-right tree)
  (if (null? tree)
      '()
      (let ((y (bstree-root tree))
            (x-node (bstree-left-branch tree))
            (right-branch (bstree-right-branch tree)))
        (if (null? x-node)
            tree
            (let ((x (bstree-root x-node))
                  (A (bstree-left-branch x-node))
                  (B (bstree-right-branch x-node)))
              (bstree-node x A (bstree-node y B right-branch)))))))

(define (bstree-balance tree)
  (if (null? tree)
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

(define (bstree-join x left right)
  (bstree-balance (bstree-node x left right)))

(define (list->bstree sorted-list)
  (if (null? sorted-list)
      sorted-list
      (car (bstree-partial sorted-list (length sorted-list)))))

(define (bstree->list tree)
  (cond
    ((null? tree) tree)
    ((null? (cdr tree)) (list (car tree)))
    (else
      (let ((root (bstree-root tree))
            (left (bstree-left-branch tree))
            (right (bstree-right-branch tree)))
        (append (bstree->list left)
          (cons root (bstree->list right)))))))

(define (bstree . l)
  ; Structure should be tolerant of duplicates, but its more efficient to just remove them.
  ; Sort then unique is more efficient if not using hashing
  (bstree-balance (list->bstree (unique (sort-ascending l)))))

(define (bstree-insert tree x)
  (if (null? tree)
    (bstree-node x '() '()) ;; if tree empty return a leaf: (x)
    (let ((root (bstree-root tree))
          (left (bstree-left-branch tree))
          (right (bstree-right-branch tree)))
       (cond
         ((= x root) tree) ;; if x = root just return tree, to avoid duplicates
         ((< x root) (bstree-join root (bstree-insert left x) right))
         (else (bstree-join root left (bstree-insert right x)))))))

(define (bstree-append tree l)
  (if (null? l)
    tree
    (bstree-append (bstree-insert tree (car l)) (cdr l))))

(define (bstree-split tree k)
  (if (null? tree)
      (cons '() '())
      (let ((root (bstree-root tree))
            (left (bstree-left-branch tree))
            (right (bstree-right-branch tree)))
        (cond
          ((= k root) (cons left right))
          ((< k root)
           (let* ((result (bstree-split left k))
                  (small (car result))
                  (big (cdr result)))
             (cons small (bstree-join root big right))))
          (else
           (let* ((result (bstree-split right k))
                  (small (car result))
                  (big (cdr result)))
             (cons (bstree-join root left small) big)))))))

(define (bstree-merge t1 t2)
  (cond
    ((null? t1) t2)
    ((null? t2) t1)
    ((= (bstree-root t1) (bstree-root t2))
     (bstree-join (bstree-root t1)
                  (bstree-merge (bstree-left-branch t1) (bstree-left-branch t2))
                  (bstree-merge (bstree-right-branch t1) (bstree-right-branch t2))))
    (else
     (let* ((sized-trees (if (< (bstree-height t1) (bstree-height t2)) (cons t1 t2) (cons t2 t1)))
            (small (car sized-trees))
            (big (cdr sized-trees))
            (pivot (bstree-root big))
            (big-left (bstree-left-branch big))
            (big-right (bstree-right-branch big))
            (split-result (bstree-split small pivot))
            (small-lt (car split-result))
            (small-gt (cdr split-result)))
       (bstree-join pivot
                    (bstree-merge small-lt big-left)
                    (bstree-merge small-gt big-right))))))

(define (bstree-contains? tree x)
  (cond
    ((null? tree) #f)
    (else
      (let ((root (car tree)))
        (cond
          ((= x root) #t)
          ((null? (cdr tree)) #f)
          (else
            (if (< x root)
                (bstree-contains? (cadr tree) x)
                (bstree-contains? (cddr tree) x))))))))

; Now that we have a self-balanced bstree, the most efficient versions of these become easy
(define (set . l)
  (apply bstree l))

(define (adjoin-set x set)
  (bstree-insert set x))

(define (element-of-set? x set)
  (bstree-contains? set x))

(define (intersection-set set1 set2)
    (list->bstree (filter (lambda (x) (element-of-set? x set1)) (bstree->list set2))))

(define (union-set set1 set2) (bstree-merge set1 set2))

(define (is-subset? sub super)
  (every (lambda (x) (element-of-set? x super)) (bstree->list sub)))

(define (set-equal? s1 s2)
  (and (is-subset? s1 s2)
       (is-subset? s2 s1)))

(define test-sets
  (list
    (list (list (set 2 3 2 1 3 2 2) (set 3 3 5 4 4 4 5)) (list (set 1 2 3 4 5) (set 3)))
    (list (list (set 10 10 10 10 10 5 5 7) (set 21 1 1 32)) (list (set 1 5 7 10 21 32) (set)))
    (list (list (set 1 2) (set 1 2 2 2 2 2)) (list (set 1 2) (set 1 2)))))

(define color-green "\x1b[32m")
(define color-red   "\x1b[31m")
(define color-reset "\x1b[0m")

(define (run-tests)
  (for-each
   (lambda (test-case)
     (let* ((inputs (car test-case))
            (expected (cadr test-case))
            (s1 (car inputs))
            (s2 (cadr inputs))
            (exp-union (car expected))
            (exp-inter (cadr expected))
            (res-union (union-set s1 s2))
            (res-inter (intersection-set s1 s2)))
       (display "Testing sets: ") (display s1) (display " and ") (display s2) (newline)
       (display "  Union: ")
       (display res-union)
       (display " (")
       (if (set-equal? res-union exp-union)
           (begin (display color-green) (display "PASS") (display color-reset))
           (begin (display color-red) (display "FAIL") (display color-reset)
                  (display " (Expected ") (display exp-union) (display " got ") (display res-union) (display ")")))
       (display ")")
       (newline)
       (display "  Intersection: ")
       (display res-inter)
       (display " (")
       (if (set-equal? res-inter exp-inter)
           (begin (display color-green) (display "PASS") (display color-reset))
           (begin (display color-red) (display "FAIL") (display color-reset)
                  (display " (Expected ") (display exp-inter) (display " got ") (display res-inter) (display ")")))
       (display ")")
       (newline)
       (newline)))
   test-sets))

(run-tests)
