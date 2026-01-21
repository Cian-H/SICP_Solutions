(define (deep-reverse l)
  (define (iter next acc)
    (cond
      ((null? next) acc)
      ((pair? next) (iter (cdr next) (cons (deep-reverse (car next)) acc)))
      (else next)))

  (iter l '()))

(define x (list (list 1 2) (list 3 4)))

(newline)
(display x)
(newline)
(display (reverse x))
(newline)
(display (deep-reverse x))
(newline)
