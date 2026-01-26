(load "lib/picture.scm")

(define (segment? s)
  (and (pair? s) (vect? (car s)) (vect? (cdr s))))

(define (_notsegment-error s) (_monotype-error s "segment"))

(define (guarded-segment s func-name)
  (if (segment? s) s (_notsegment-error func-name)))

(define (make-segment start-vec end-vec)
  (let ((v1 (guarded-vect start-vec "make-segment/arg1"))
        (v2 (guarded-vect end-vec "make-segment/arg2")))
    (cons v1 v2)))

(define (start-segment s)
  (car (guarded-segment s "start-segment")))

(define (end-segment s)
  (cdr (guarded-segment s "end-segment")))

(define (segment->string s)
  (string-append "[" (vect->string (start-segment s)) "-" (vect->string (end-segment s)) "]"))

(define (display-segment s)
  (display (segment->string s)))
