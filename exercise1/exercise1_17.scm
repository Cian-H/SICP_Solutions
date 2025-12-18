(define (even? n)
  (= (remainder n 2) 0))

(define (fast-mult a b)
  (cond
    ((= b 0) 0)
    ((even? b) (+ (fast-mult a (/ b 2)) (fast-mult a (/ b 2))))
    (else (+ a (fast-mult a (- b 1))))))
