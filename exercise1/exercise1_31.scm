(load "lib/math.scm")

(define (product term a next b)

  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))

  (iter a 1))

(define (identity x) x)

(define (inc i) (+ i 1))

(define (factorial n) (product identity 1 inc n))

(define (calc-pi n)

  (define (half-inc x)
    (+ x 0.5))

  (define (pi-term x)
    (define num (* 2.0 (ceiling (- x 0.5))))
    (define den (- (* 2.0 (ceiling x)) 1.0))
    (/ num den))

  (* 4.0 (product pi-term 1.5 half-inc n)))

(display (factorial 4)) ; 24
(newline)
(display (calc-pi 10000000)) ; 3.1415927321277946
(newline)
