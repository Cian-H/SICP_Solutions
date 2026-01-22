(load "lib/functools.scm")

(define (map p s)
  (accumulate (lambda (x y) (cons (p x) y)) nil s))

(define (append s1 s2)
  (accumulate cons s2 s1))

(define (length s)
  (accumulate (lambda (x y) (+ y 1)) 0 s))

(define l (list 1 2 3 4 5))
(define m (list 6 7 8 9 10))

(define (square x) (* x x))

(newline)
(display "l => ")
(display l)
(newline)
(display "(map square l) => ")
(display (map square l))
(newline)
(display "(append l m) => ")
(display (append l m))
(newline)
(display "(length l) => ")
(display (length l))
(newline)
