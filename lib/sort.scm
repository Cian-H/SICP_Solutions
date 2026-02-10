(define (sort l pred)
  (define (merge l r acc)
    (cond ((null? l) (append-reverse acc r))
      ((null? r) (append-reverse acc l))
      (else
        (let ((lh (car l)) (lt (cdr l)) (rh (car r)) (rt (cdr r)))
          (if (pred lh rh)
            (merge lt r (cons lh acc))
            (merge l rt (cons rh acc)))))))

  (cond
    ((null? l) l)
    ((not (pair? l)) (error "`sort` only accepts pairs!"))
    ((null? (cdr l)) l)
    ((null? (cddr l)) (if (pred (car l) (cadr l)) (list (cadr l) (car l)) l))
    (else
      (let* ((parts (split-halves l))
             (left (car parts))
             (right (cdr parts))
             (left-sorted (sort left))
             (right-sorted (sort right)))
        (merge left-sorted right-sorted '())))))

(define (sort-ascending l) (sort l <=)) ; Note: must be <= for stable sort
(define (sort-descending l) (sort l >))
