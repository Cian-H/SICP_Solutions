(define (accumulate op acc s)
  (if (null? s)
    acc
    (op (car s) (accumulate op acc (cdr s)))))

(define (accumulate-n op acc seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op acc (map car seqs))
      (accumulate-n op acc (map cdr seqs)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(define (every pred seq)
  (cond
    ((null? seq) #t)
    ((pred (car seq)) (every pred (cdr seq)))
    (else #f)))

(define (count pred seq)
  (fold-left (lambda (acc x) (if (pred x) (+ acc 1) acc)) 0 seq))

(define (frequencies lst pred)
  (define (pair-pred a b)
    (pred (car a) (car b)))

  (define (update-tree tree x)
    (let* ((dummy-key (cons x 0))
           (existing-node (bstree-lookup dummy-key car tree)))
      (if existing-node
        (let* ((count (cdr existing-node))
               (new-node (cons x (+ count 1))))
          (bstree-insert (bstree-remove tree existing-node) new-node))
        (bstree-insert tree (cons x 1)))))

  (let ((result-tree (fold-left update-tree (bstree-make-empty pair-pred) lst)))
    (bstree->list result-tree)))
