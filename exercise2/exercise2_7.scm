(define (make-interval a b) (cons a b))

(define (predicate-selector f)
  (lambda (i)
    (let ((i0 (car i))
          (i1 (cdr i)))
      (if (f i0 i1) i0 i1))))

(define lower-bound (predicate-selector <))
(define upper-bound (predicate-selector >))
