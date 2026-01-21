(define (reverse l)
  (define (iter next acc)
    (if (null? next)
      acc
      (iter (cdr next) (cons (car next) acc))))

  (iter l '()))

(display (reverse (list 1 4 9 16 25)))
