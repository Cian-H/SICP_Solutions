(define (treesort l pred)
  (define (insert t item)
    (if (null? t)
      (list item '() '())
      (let ((node (car t))
            (left (cadr t))
            (right (caddr t)))
        (if (pred item node)
          (list node (insert left item) right)
          (list node left (insert right item))))))

  (define (flatten t acc)
    (if (null? t)
      acc
      (flatten (cadr t) (cons (car t) (flatten (caddr t) acc)))))

  (cond
    ((or (null? l) (null? (cdr l))) l)
    ((null? (cddr l)) (sort2 l pred))
    ((null? (cdddr l)) (sort3 l pred))
    (else
      (flatten (fold-left insert '() l) '()))))
