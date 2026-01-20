(define (sort-interval x) (cons (lower-bound-interval x) (upper-bound-interval x)))

(define (make-interval a b) (sort-interval (cons a b)))

(define (make-interval-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-interval-center-percent c p)
  (if (zero? c)
    (error "Cannot construct an interval from a percentage where centerpoint is 0"))
  (make-interval-center-width c (abs (* c p))))

(define (predicate-selector f)
  (lambda (i)
    (let ((i0 (car i))
          (i1 (cdr i)))
      (if (f i0 i1) i0 i1))))

(define lower-bound-interval (predicate-selector <))
(define upper-bound-interval (predicate-selector >))

(define (interval->string i)
  (let ((i0 (number->string (lower-bound-interval i)))
        (i1 (number->string (upper-bound-interval i))))
    (string-append "[" i0 ", " i1 "]")))
