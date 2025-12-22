(define (smoother dx)
  (lambda (f)
    (lambda (x)
      (let ((y1 (f (- x dx)))
            (y2 (f x))
            (y3 (f (+ x dx))))
        (/ (+ y1 y2 y3) 3.0)))))

(define (smooth f dx)
  ((smoother dx) f))

(define (smooth-n-fold f n dx)
  ((repeated (smoother dx) n) f))
