(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter g n)
    (cond
      ((zero? n) identity)
      ((> n 1) (iter (compose f g) (- n 1)))
      (else g)))

  (iter f n))
