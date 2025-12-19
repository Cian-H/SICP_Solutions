(load "lib/math.scm")

(define (fixed-point f first-guess)

  (define (try guess)
    (display (string-append "Guess: " (number->string guess)))
    (newline)
    (let ((next (f guess)))
      (if (is-close? guess next)
        next
        (try next))))

  (try first-guess))

(display (string-append
          "Converged at: "
          (number->string
            (fixed-point
              (lambda (x) (/ (log 1000.0) (log x)))
              2.0))))
(newline)
