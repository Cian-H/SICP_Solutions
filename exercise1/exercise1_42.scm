(load "lib/math.scm")

(define (compose f g)
  (lambda (x) (f (g x))))

(display ((compose square inc) 6)) ; => 49 ; Basically does `(square (inc 6))`, i.e: 7^2
