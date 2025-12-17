(define (pascals-triangle depth)
  (define (next-layer current-layer)
    (define (layer-iter prev remaining-current built-next)
      (if (null? remaining-current)
        (reverse (cons 1 built-next))
        (layer-iter
          (car remaining-current)
          (cdr remaining-current)
          (cons (+ (car remaining-current) prev) built-next))))

    (layer-iter 0 current-layer '()))

  (define (triangle-iter i current-rows)
    (if (= i 1)
      (reverse current-rows)
      (triangle-iter
        (- i 1)
        (cons (next-layer (car current-rows)) current-rows))))

  (triangle-iter depth '((1))))
