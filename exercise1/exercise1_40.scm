(load "lib/math.scm")

(define (cubic a b c)
  (let* ((equation (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
         (left-zero (newtons-method equation -1290))
         (right-zero (newtons-method equation 1290))
         (mid-zero (newtons-method equation (/ (+ left-zero right-zero) 2))))
    (list left-zero mid-zero right-zero)))

(display (cubic 4.0 2.0 -2.0))
