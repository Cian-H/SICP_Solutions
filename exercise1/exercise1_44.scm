(load "lib/math.scm")

; Deviating from the original text here but for good reason.
; Hardcoding dx just seems like a shitty idea, so currying dx
; into the function instead seems like a much better architecture
; for making this flexible and reliable
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
