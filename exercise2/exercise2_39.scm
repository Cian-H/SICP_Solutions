(load "lib/functools.scm")

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(define l (list 1 2 3 4 5 6 7 8 9))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence)) ; This append triggers traversal to end node for every node it visits

(newline)
(display (reverse l))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence)) ; Much cleaner and more performant

(newline)
(display (reverse l))
(newline)
