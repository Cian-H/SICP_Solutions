(load "lib/math.scm")

(define (double f)
  (lambda (x) (f (f x))))

(display (((double (double double)) inc) 5)) ; => 21 ; Basically does `inc` 2^2^2 times, i.e 16 times
