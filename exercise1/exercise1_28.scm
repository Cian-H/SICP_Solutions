(load "lib/math.scm")

(define (mr-prime? n times)

  (define (expmod base exp m)
    (cond
      ((= exp 0) 1)
      ((even? exp
          (let ((x (expmod base (/ exp 2) m)))
            (if (= x 0)
              0
              (check-nontrivial-sqrt x m)))))
      (else
        (let ((x (expmod base (- exp 1) m)))
          (if (= x 0)
            0
            (remainder (* base x) m))))))

  (define (check-nontrivial-sqrt x m)
    (let ((r (remainder (square x) m)))
      (if (and (= r 1)
           (not (= x 1))
           (not (= x (- m 1))))
        0
        r)))

  (define (try-it a)
    (= (expmod a (- n 1) n) 1))

  (define (miller-rabin-test n)
    (try-it (+ 1 (random (- n 1)))))

  (cond
    ((= times 0) true)
    ((miller-rabin-test n) (mr-prime? n (- times 1)))
    (else false)))
