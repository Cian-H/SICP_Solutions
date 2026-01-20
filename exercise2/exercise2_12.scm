(load "lib/intervals.scm")

(define (make-interval-center-percent c p)
  (make-interval-center-width c (abs (* c p))))

(define (percent-interval i)
  (let ((c (center-interval i)))
    (abs (/ (- c (lower-bound-interval i)) c))))

(define i (make-interval-center-percent 10 0.1))
(define neg-i (make-interval-center-percent -10 0.1))

(display "~~~ Testing ~~~")
(newline)
(display (string-append "i = " (interval->string i)))
(newline)
(display (string-append "Percent i = " (number->string (percent-interval i))))
(newline)
(display (string-append "neg-i = " (interval->string neg-i)))
(newline)
(display (string-append "Percent neg-i = " (number->string (percent-interval neg-i))))
(newline)
