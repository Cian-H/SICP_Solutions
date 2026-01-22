(load "lib/functools.scm")

(define (horner-eval x s)
  (accumulate (lambda (i acc) (+ i (* x acc))) 0 s))

(display (horner-eval 2 (list 1 3 0 5 0 1)))
