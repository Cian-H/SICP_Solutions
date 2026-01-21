(define (same-parity xh . xt)
  (define (all-int? l)
    (cond
      ((null? l) #t)
      ((integer? (car l)) (all-int? (cdr l)))
      (else #f)))

  (if (not (and (integer? xh) (all-int? xt)))
    (error "args must all be integers!"))

  (define predicate? (if (even? xh) even? odd?))

  (define (iter y acc)
    (if (null? y)
      acc
      (let ((yh (car y))
            (yt (cdr y)))
        (if (predicate? yh)
          (iter yt (cons yh acc))
          (iter yt acc)))))

  (reverse (iter xt (list xh))))

(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
(newline)
