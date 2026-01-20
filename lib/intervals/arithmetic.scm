(define (add-interval x y)
  (make-interval
    (+ (lower-bound-interval x) (lower-bound-interval y))
    (+ (upper-bound-interval x) (upper-bound-interval y))))

(define (sub-interval x y)
  (add-interval
    x
    (make-interval
      (- (upper-bound-interval y))
      (- (lower-bound-interval y)))))

(define (mul-interval x y)
  (let ((x0 (lower-bound-interval x))
        (x1 (upper-bound-interval x))
        (y0 (lower-bound-interval y))
        (y1 (upper-bound-interval y)))
    (cond
      ((pos-or-zero? x0) (cond
                          ((pos-or-zero? y0) (make-interval (* x0 y0) (* x1 y1)))
                          ((neg-or-zero? y1) (make-interval (* x1 y0) (* x0 y1)))
                          (else (make-interval (* x1 y0) (* x1 y1)))))
      ((neg-or-zero? x1) (cond
                          ((pos-or-zero? y0) (make-interval (* x0 y1) (* x1 y0)))
                          ((neg-or-zero? y1) (make-interval (* x1 y1) (* x0 y0)))
                          (else (make-interval (* x0 y1) (* x0 y0)))))
      (else (cond
             ((pos-or-zero? y0) (make-interval (* x0 y1) (* x1 y1)))
             ((neg-or-zero? y1) (make-interval (* x1 y0) (* x0 y0)))
             (else (make-interval (min (* x0 y1) (* x1 y0)) (max (* x0 y0) (* x1 y1)))))))))

(define (recip-interval i)
  (make-interval (/ 1.0 (upper-bound-interval i)) (/ 1.0 (lower-bound-interval i))))

(define (div-interval x y)
  (if (spans-zero? y)
    (error "Interval y cannot span zero!")
    (mul-interval x (recip-interval y))))
