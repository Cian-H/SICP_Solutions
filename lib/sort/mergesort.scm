(define (sort:merge left right pred)
  (define (iter l r acc)
    (cond ((null? l) (append-reverse acc r))
      ((null? r) (append-reverse acc l))
      (else
        (let ((lh (car l)) (lt (cdr l)) (rh (car r)) (rt (cdr r)))
          (if (pred lh rh)
            (iter lt r (cons lh acc))
            (iter l rt (cons rh acc)))))))

  (iter left right '()))

(define (mergesort l pred)
  (cond
    ((null? l) l)
    ((not (pair? l)) (error "`sort` only accepts pairs!"))
    ((null? (cdr l)) l)
    ((null? (cddr l)) (if (pred (car l) (cadr l)) l (list (cadr l) (car l))))
    (else
      (let* ((parts (split-halves l))
             (left (car parts))
             (right (cdr parts))
             (left-sorted (mergesort left pred))
             (right-sorted (mergesort right pred)))
        (sort:merge left-sorted right-sorted pred)))))
