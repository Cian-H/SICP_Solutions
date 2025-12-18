(define (average x y)
  (/ (+ x y) 2))

(define (fast-mult x n)

  (define (iter b i a)
    (cond
      ((= i 0) a)
      ((even? i) (iter (+ b b) (/ i 2) a))
      (else (iter b (- i 1) (+ a b)))))

  (iter x n 0))

(define (square x)
  (* x x))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
        (+ counter 1))))

  (iter 1 1))

(define (expt x n)

  (define (iter b i a)
    (cond
      ((= i 0) a)
      ((even? i) (iter (* b b) (/ i 2) a))
      (else (iter b (- i 1) (* a b)))))

  (iter x n 1))
