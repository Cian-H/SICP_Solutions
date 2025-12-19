(load "lib/math.scm")

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0) ; 1.6180339889579018
