(define (integral f a b dx)

  (define (add-dx x)
    (+ x dx))

  (* (sum f (+ a (/ dx 1.0)) add-dx b) dx))
