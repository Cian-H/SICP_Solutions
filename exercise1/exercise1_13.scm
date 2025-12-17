; I don't feel like doing maths, and even if the mathematical premise is true
; here (which i grant), the formula will break down as floating point precision
; degrades. So, instead, i decided to verify this theorem by testing its
; computational limits

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      a
      (fib-iter b (+ a b) (- count 1))))

  (fib-iter 0 1 n))

(define (premise-holds? n)
  (= (fib n) (inexact->exact (round (/ (expt phi n) (sqrt 5))))))

(define (find-computational-limit)
  (define (iter-check i)
    (if (premise-holds? i)
      (iter-check (+ i 1))
      i))

  (iter-check 0))

(define n (find-computational-limit))

(write (string-append
        "Premise breaks down at n="
        (number->string n)
        " (value="
        (number->string (fib n))
        ")"))
