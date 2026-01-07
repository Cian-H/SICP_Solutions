(define (divides? a b)
  (zero? (remainder b a)))

(define (even? n)
  (divides? 2 n))

(define (is-close? a b)
  (define (is-close-tolerance? a b rel-tol abs-tol)
    (let ((diff (abs (- a b)))
          (mag-a (abs a))
          (mag-b (abs b)))
      (<= diff
        (max (* rel-tol (max mag-a mag-b))
          abs-tol))))

  (is-close-tolerance? a b default-rel-tol default-abs-tol))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (fermat-prime? n times)

  (define (expmod base exp m)
    (cond
      ((= exp 0) 1)
      ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
      (else (remainder (* base (expmod base (- exp 1) m)) m))))

  (define (try-it a)
    (= (expmod a n n) a))

  (define (fermat-test n)
    (try-it (+ 1 (random (- n 1)))))

  (cond
    ((zero? times) true)
    ((fermat-test n) (fermat-prime? n (- times 1)))
    (else false)))

(define (mr-prime? n times)

  (define (expmod base exp m)
    (cond
      ((= exp 0) 1)
      ((even? exp) (let ((x (expmod base (/ exp 2) m)))
                    (if (= x 0)
                      0
                      (check-nontrivial-sqrt x m))))
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
