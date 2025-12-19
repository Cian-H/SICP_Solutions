(load "lib/math.scm")

(define (tan-cf x k)

  (define x-sq (square x))

  (define (a i)
    (- x-sq))

  (define (d i)
    (+ 1 (* i 2)))

  (/ x (+ 1 (cont-frac a d k))))

(display (tan-cf 0.5 100)) ; 0.5463024898437905
(newline)
