(define (center-interval i)
  (/ (+ (lower-bound-interval i) (upper-bound-interval i)) 2))

(define (width-interval x)
  (/ (- (upper-bound-interval x) (lower-bound-interval x)) 2))
