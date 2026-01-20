(define (spans-zero? x)
  (<= (* (lower-bound-interval x) (upper-bound-interval x)) 0))
