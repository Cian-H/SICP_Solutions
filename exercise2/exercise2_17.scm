(define (last-pair l)
  (let ((next (cdr l)))
    (if (null? next)
      (car l)
      (last-pair next))))

(display (last-pair (list 23 72 149 34)))
