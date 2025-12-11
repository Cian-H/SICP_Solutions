(define (square x)
  (* x x))

(define (sum_of_squares a b)
  (+ (square a) (square b)))

(define (sum_of_largest_squares a b c)
  (cond ((and (< a c) (< a b)) (sum_of_squares b c))
    ((and (< b a) (< b c)) (sum_of_squares a c))
    (else (sum_of_squares a b))))

(write (sum_of_largest_squares 1 2 3))
