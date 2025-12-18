(load "lib/math.scm")

(define carmichael-numbers '(561 1105 1729 2465 2821 6601))

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (congruent? n)
  (define (iter a)
    (cond
      ((< a 2) true)
      ((= (expmod a n n) a) (iter (- a 1)))
      (else false)))

  (iter (- n 1)))

(define (test-carmichael-number n)
  (if (congruent? n)
    (display (string-append "The number " (number->string n) " IS a Carmichael number"))
    (display (string-append "The number " (number->string n) " is NOT a Carmichael number"))))

(define (run l)
  (if (not (null? l))
    (list (test-carmichael-number (car l))
      (newline)
      (run (cdr l)))))

(run carmichael-numbers)
