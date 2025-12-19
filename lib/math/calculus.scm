(define (integral f a b n)

  (define h (/ (- b a) n))

  (define (yk k)
    (f (+ a (* k h))))

  (define (series-component coeff)
    (lambda (k)
      (* coeff (yk k))))

  (define (next k)
    (+ k 2))

  (*
    (/ h 3)
    (+ (sum (series-component 4) 1 next (- n 1))
      (sum (series-component 2) 2 next (- n 1))
      (yk 0)
      (yk n))))
