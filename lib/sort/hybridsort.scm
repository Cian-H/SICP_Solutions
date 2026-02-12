(define (hybridsort l pred)
  (define cutoff 32)

  (define (sort-strategy l n)
    (cond
      ((<= n 1) l)
      ((= n 2) (sort2 l pred))
      ((= n 3) (sort3 l pred))
      ((< n cutoff) (insertionsort l pred))
      (else
        (let* ((n-left (quotient n 2))
               (n-right (- n n-left))
               (parts (split l n-left))
               (left (sort-strategy (car parts) n-left))
               (right (sort-strategy (cdr parts) n-right)))
          (sort:merge left right pred)))))

  (sort-strategy l (length l)))
