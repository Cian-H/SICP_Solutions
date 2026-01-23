(load "lib/functools.scm")

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(define l (list 1 2 3))

(newline)
(display (fold-right / 1 l)) ; => 3/2
(newline)
(display (fold-left / 1 l)) ; => 1/6
(newline)
(display (fold-right list nil l)) ; => (1 (2 (3 ())))
(newline)
(display (fold-left list nil l)) ; => (((() 1) 2) 3)
(newline)
