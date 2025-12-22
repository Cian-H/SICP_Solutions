(load "lib/math.scm")

(define (nth-root x n)
  (define (iter n i)
    (if (> n 2)
      (iter (/ n 2) (+ i 1))
      i))

  (fixed-point
    ((repeated average-damp (iter n 1)) (lambda (y) (/ x (expt y (- n 1)))))
    1.0))

(define (test x-min x-max n-min n-max)
  (define (test-x current-x current-n)
    (if (<= current-x x-max)
      (begin
        (display
          (string-append "n = " (number->string current-n) ", x = " (number->string current-x)
            ", root = "
            (number->string (nth-root (exact->inexact current-x) current-n))))
        (newline)
        (test-x (+ current-x 1) current-n))))

  (define (test-n current-n)
    (if (<= current-n n-max)
      (begin
        (test-x x-min current-n)
        (newline)
        (test-n (+ current-n 1)))))

  (test-n n-min))

(test 2 25 2 20)
