(define (identity x) x)

(define (inc i) (+ i 1))

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

(define (factorial n) (product identity 1 inc n))

(define (expt x n)

  (define (iter b i a)
    (cond
      ((= i 0) a)
      ((even? i) (iter (* b b) (/ i 2) a))
      (else (iter b (- i 1) (* a b)))))

  (iter x n 1))
