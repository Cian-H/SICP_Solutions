(define (integral f a b n)

  (define h (/ (- b a) n))

  (define (series-prefix k)
    (cond
      ((or (zero? k) (= k n)) 1)
      ((even? k) 2)
      (else 4)))

  (define (yk k)
    (f (+ a (* k h))))

  (define (series k)
    (* (series-prefix k) (yk k)))

  (define (next k)
    (+ k 1))

  (*
    (/ h 3)
    (sum series 0 next n)))
