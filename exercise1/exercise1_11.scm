(define (f-rec n)
  (if (< n 3)
    n
    (+
      (f-rec (- n 1))
      (* 2 (f-rec (- n 2)))
      (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (f-loop a b c i)
    (if (= i 0)
      a
      (f-loop (+ a (* 2 b) (* 3 c)) a b (- i 1))))

  (if (< n 3)
    n
    (f-loop 2 1 0 (- n 2))))
