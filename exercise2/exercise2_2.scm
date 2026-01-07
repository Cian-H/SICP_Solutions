(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define make-point cons)

(define x-point car)

(define y-point cdr)

(define point? pair?)

(define (make-segment p1 p2)
  (if (and (point? p1) (point? p2))
    (cons p1 p2)
    (error "Arguments for `make-segment` must be points")))

(define (segment? l)
  (and (pair? l) (point? (car l)) (point? (cdr l))))

(define (segment-func f)
  (lambda (l)
    (if (segment? l)
      (f l)
      (error "Argument must be a segment"))))

(define start-segment (segment-func car))

(define end-segment (segment-func cdr))

(define (print-segment l)
  (display "(")
  (print-point (start-segment l))
  (display ",")
  (print-point (end-segment l))
  (display ")"))

(define (midpoint-segment l)
  (let ((p1 (start-segment l))
        (p2 (end-segment l))
        (x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))
