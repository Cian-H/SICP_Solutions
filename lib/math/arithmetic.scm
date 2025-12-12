(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
        (+ counter 1))))

  (iter 1 1))
