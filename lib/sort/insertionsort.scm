(define (insertionsort l pred)
  (define (insert item sorted acc)
    (cond
      ((null? sorted) (append-reverse acc (list item)))
      ((pred item (car sorted)) (append-reverse acc (cons item sorted)))
      (else (insert item (cdr sorted) (cons (car sorted) acc)))))

  (if (null? l) '() (fold-left (lambda (acc x) (insert x acc '())) '() l)))
