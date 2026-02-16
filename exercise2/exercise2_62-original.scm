(load "lib/functools.scm")
(load "lib/sort.scm")

; I think it's asking for a binary search tree here. This will simplify `element-of-set`
; and `union-set` pretty naturally
(define (list->bstree sorted-list)
  (let ((len (length sorted-list)))
    (cond
      ((= len 0) '())
      ((= len 1) (list (car sorted-list)))
      (else
        (let* ((mid-index (quotient len 2))
               (left-part (take sorted-list mid-index))
               (root (list-ref sorted-list mid-index))
               (right-part (drop sorted-list (+ mid-index 1))))
          (cons root
            (cons (list->bstree left-part)
              (list->bstree right-part))))))))

(define (bstree->list tree)
  (cond
    ((null? tree) '())
    ((null? (cdr tree)) (list (car tree)))
    (else
      (let ((root (car tree))
            (left (cadr tree))
            (right (cddr tree)))
        (append (bstree->list left)
          (cons root (bstree->list right)))))))

(define (bstree-balance tree)
  (list->bstree (bstree->list tree)))

(define (bstree . l) (list->bstree (sort-ascending l)))

(define (bstree-append tree l)
  (if (null? l)
    tree
    (list->bstree (sort-ascending (append (bstree->list tree) l)))))

(define (bstree-merge tree other-tree)
  (bstree-append tree (bstree->list other-tree)))

(define (append-to-bstree tree l)
  (if (null? l)
    tree
    (append-to-bstree (bstree-insert tree (car l)) (cdr l))))

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

; Now, since we're a bstree, these become easy
(define (set . l)
  (apply bstree l))

(define (adjoin-set x set)
  (bstree-insert set x))

(define (element-of-set? x set)
  (bstree-contains? set x))

(define (intersection-set set1 set2)
    (list->bstree (filter (lambda (x) (element-of-set? x set1)) (bstree->list set2))))

(define (union-set set1 set2) (bstree-merge set1 set2))

(define test-sets
  (list
    (list (list (set 2 3 2 1 3 2 2) (set 3 3 5 4 4 4 5)) (list (set 1 2 3 4 5) (set 3)))
    (list (list (set 10 10 10 10 10 5 5 7) (set 21 1 1 32)) (list (set 1 5 7 10 21 32) (set)))
    (list (list (set 1 2) (set 1 2 2 2 2 2)) (list (set 1 2) (set 1 2)))))

(define color-green "\x1b[32m")
(define color-red   "\x1b[31m")
(define color-reset "\x1b[0m")

(define (is-subset? sub super)
  (every (lambda (x) (element-of-set? x super)) (bstree->list sub)))

(define (set-equal? s1 s2)
  (and (is-subset? s1 s2)
       (is-subset? s2 s1)))

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
