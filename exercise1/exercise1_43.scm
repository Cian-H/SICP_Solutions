(load "lib/math.scm")

(define (repeated f n)
  (define (iter g n)
    (if (> n 1)
      (iter (compose f g) (- n 1))
      g))

  (iter f n))

(display ((repeated square 2) 5)) ; => 625
