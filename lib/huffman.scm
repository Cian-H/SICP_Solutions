(load "lib/safe-load.scm")
(load "lib/functools.scm")
(load "lib/sort.scm")

(define tag-enctree 'enctree)
(define tag-enctree-leaf 'enctree-leaf)

(define (enctree-tag tree) (car tree))
(define (enctree-tree tree) (cdr tree))
(define (enctree-root tree) (car (enctree-tree tree)))
(define (enctree-root-val tree) (car (enctree-root tree)))
(define (enctree-root-weight tree) (cdr (enctree-root tree)))
(define (enctree-branches tree) (cdr (enctree-tree tree)))
(define (enctree-left-branch tree) (car (enctree-branches tree)))
(define (enctree-right-branch tree) (cdr (enctree-branches tree)))

(define (enctree? tree) (eq? (enctree-tag tree) tag-enctree))
(define (enctree-leaf? tree) (eq? (enctree-tag tree) tag-enctree-leaf))
(define (enctree-node? tree) (or (enctree? tree) (enctree-leaf? tree)))

(define (make-enctree-leaf val weight)
  (cons tag-enctree-leaf
    (cons (cons val weight) '())))

(define (make-enctree left right)
  (let ((left-val (enctree-root-val left))
        (left-weight (enctree-root-weight left))
        (right-val (enctree-root-val right))
        (right-weight (enctree-root-weight right)))
    (cons
      tag-enctree
      (cons
        (cons
          (cond
            ((and (enctree-leaf? left) (enctree-leaf? right)) (list left-val right-val))
            ((enctree-leaf? left) (cons left-val right-val))
            ((enctree-leaf? right) (cons right-val left-val))
            (else (append left-val right-val)))
          (+ left-weight right-weight))
        (cons left right)))))

(define (insert-enctree tree tree-list)
  (cond
    ((null? tree-list) (list tree))
    ((< (enctree-root-weight tree) (enctree-root-weight (car tree-list)))
      (cons tree tree-list))
    (else (cons (car tree-list) (insert-enctree tree (cdr tree-list))))))

(define (list->enctree-leaves l)
  (if (null? l)
    l
    (let ((head (car l))
          (tail (cdr l)))
      (if (pair? head)
        (cons (make-enctree-leaf (car head) (cdr head)) (list->enctree-leaves tail))
        (error "`list->enctree-leaves` only accepts a list of pairs!")))))

(define (build-enctree tree-list)
  (cond
    ((null? tree-list) '())
    ((null? (cdr tree-list)) (car tree-list))
    (else
      (let* ((left (car tree-list))
             (right (cadr tree-list))
             (merged (make-enctree left right))
             (rest (cddr tree-list)))
        (build-enctree (insert-enctree merged rest))))))

(define (general-freq l)
  (define (update-count val acc)
    (cond
      ((null? acc) (list (cons val 1)))
      ((eq? val (caar acc)) (cons (cons val (+ (cdar acc) 1)) (cdr acc)))
      (else (cons (car acc) (update-count val (cdr acc))))))

  (define (iter tail acc)
    (if (null? tail)
      acc
      (let ((th (car tail))
            (tt (cdr tail)))
        (iter tt (update-count th acc)))))

  (iter l '()))

(define (list->enctree l)
  (let* ((freqs (general-freq l))
         (leaves (map (lambda (pair) (make-enctree-leaf (car pair) (cdr pair))) freqs)))
    (build-enctree leaves)))

(define (letter-freq msg)
  (sort (frequencies (string->list msg) char<=? char=?) (lambda (x y) (<= (cdr x) (cdr y)))))

(define (string->enctree msg)
  (if (string=? msg "")
    '()
    (let* ((freqs (letter-freq msg))
           (leaves (map (lambda (pair) (make-enctree-leaf (car pair) (cdr pair))) freqs)))
      (build-enctree leaves))))

(define (char-in-tree? char tree)
  (let ((symbols (enctree-root-val tree)))
    (if (enctree-leaf? tree)
      (eq? char symbols)
      (and (memv char symbols) #t))))

(define (encode-char char tree)
  (if (enctree-leaf? tree)
    '()
    (let ((left (enctree-left-branch tree))
          (right (enctree-right-branch tree)))
      (cond
        ((char-in-tree? char left) (cons 0 (encode-char char left)))
        ((char-in-tree? char right) (cons 1 (encode-char char right)))
        (else (error "Character not found in tree:" char))))))

(define (encode-list char-list tree)
  (if (null? char-list)
    '()
    (append (encode-char (car char-list) tree)
      (encode-list (cdr char-list) tree))))

(define (encode-string msg tree)
  (apply string-append (map number->string (encode-list (string->list msg) tree))))

(define (char->digit c)
  (- (char->integer c) (char->integer #\0)))

(define (string->binlist s)
  (map char->digit (string->list s)))

(define (decode-string s tree)
  (define (iter st subtree acc)
    (cond
      ((enctree-leaf? subtree)
        (let ((new-acc (cons (enctree-root-val subtree) acc)))
          (if (null? st) new-acc (iter st tree new-acc))))
      ((null? st) acc)
      (else
        (let ((sth (car st))
              (stt (cdr st)))
          (if (= sth 0)
            (iter stt (enctree-left-branch subtree) acc)
            (iter stt (enctree-right-branch subtree) acc))))))

  (list->string (reverse (iter (string->binlist s) tree '()))))
