(load "lib/math.scm")

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1)
        (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (calc-inverse-phi k)
  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0)) ; 1.6180339889579018

(define inverse-phi (/ 1.0 phi))

(define (test-k k)
  (abs (- (calc-inverse-phi k) inverse-phi)))

(define (inverse-phi-accurate-to n)
  (define exp-minus-n (/ 1.0 (expt 10.0 (+ n 1.0))))

  (define (iter i)
    (if (< (test-k i) exp-minus-n)
      i
      (iter (inc i))))

  (iter 1))

(display (inverse-phi-accurate-to 4))
(newline)
