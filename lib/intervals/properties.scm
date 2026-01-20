(define (center-interval i)
  (/ (+ (lower-bound-interval i) (upper-bound-interval i)) 2))

(define (width-interval x)
  (/ (- (upper-bound-interval x) (lower-bound-interval x)) 2))

(define (percent-interval i)
  (let ((c (center-interval i)))
    (if (zero? c)
      (error "Percent undefined for 0-center interval" i)
      (abs (/ (- c (lower-bound-interval i)) c)))))
