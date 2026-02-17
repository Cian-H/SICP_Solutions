(load "lib/functools.scm")
(load "lib/sort.scm")

; Originally, i thought exercise 2.62 was asking for a btree, but i later realised that exercise
; wanted a binary search instead. So, i've kept my binary search tree implementation from 2.62
; and i'm re-using it here instead of the implementation in SICP.
(define (bstree-node val left right)
  (if (and (null? left) (null? right))
      (cons val '())
      (cons val (cons left right))))

(define (bstree-root tree) (car tree))
(define (bstree-branches tree) (cdr tree))

(define (bstree-left-branch tree)
  (if (null? (cdr tree)) '() (cadr tree)))

(define (bstree-right-branch tree)
  (if (null? (cdr tree)) '() (cddr tree)))

(define (bstree-height tree)
  (if (null? tree)
      0
      (+ 1 (max (bstree-height (bstree-left-branch tree))
                (bstree-height (bstree-right-branch tree))))))

(define (bstree-partial elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (bstree-partial elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (this-entry (car non-left-elts))
             (right-size (- n (+ left-size 1)))
             (right-result (bstree-partial (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
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
      '()
      (let ((bf (- (bstree-height (bstree-left-branch tree))
                   (bstree-height (bstree-right-branch tree)))))
        (cond
          ((> bf 1)
           (if (< (bstree-height (bstree-left-branch tree))
                  (bstree-height (bstree-right-branch (bstree-left-branch tree))))
               (bstree-rotate-right (bstree-node (bstree-root tree)
                                                 (bstree-rotate-left (bstree-left-branch tree))
                                                 (bstree-right-branch tree)))
               (bstree-rotate-right tree)))
          ((< bf -1)
           (if (> (bstree-height (bstree-right-branch tree))
                  (bstree-height (bstree-left-branch (bstree-right-branch tree))))
               (bstree-rotate-left (bstree-node (bstree-root tree)
                                                (bstree-left-branch tree)
                                                (bstree-rotate-right (bstree-right-branch tree))))
               (bstree-rotate-left tree)))
          (else tree)))))

(define (list->bstree sorted-list)
  (if (null? sorted-list)
      '()
      (car (bstree-partial sorted-list (length sorted-list)))))

(define (bstree->list tree)
  (cond
    ((null? tree) '())
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
         ((< x root) (bstree-balance (bstree-node root (bstree-insert left x) right)))
         (else (bstree-balance (bstree-node root left (bstree-insert right x))))))))

(define (bstree-append tree l)
  (if (null? l)
    tree
    (bstree-append (bstree-insert tree (car l)) (cdr l))))

(define (bstree-merge tree other-tree)
  (bstree-append tree (bstree->list other-tree)))

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
