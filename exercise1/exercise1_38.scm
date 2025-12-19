(load "lib/math.scm")

(define (calc-e k)

  (define (d i)
    (let ((j (+ i 1)))
      (if (zero? (remainder j 3))
        (/ j 1.5)
        1.0)))

  (+ 2 (cont-frac (lambda (i) 1.0) d k)))

(display (calc-e 100)) ; 2.7182818284590453
(newline)
