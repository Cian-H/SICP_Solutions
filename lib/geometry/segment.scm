(define (make-segment p1 p2)
  (if (and (point? p1) (point? p2))
    (cons p1 p2)
    (error "Arguments for `make-segment` must be points")))

(define (segment? l)
  (and (pair? l) (point? (car l)) (point? (cdr l))))

(define (print-segment l)
  (display "(")
  (print-point (start-segment l))
  (display ",")
  (print-point (end-segment l))
  (display ")"))

(define (midpoint-segment l)
  (let* ((p1 (start-segment l))
         (p2 (end-segment l))
         (x1 (x-point p1))
         (y1 (y-point p1))
         (x2 (x-point p2))
         (y2 (y-point p2)))
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(define (x-start-segment l)
  (x-point (start-segment l)))

(define (y-start-segment l)
  (y-point (start-segment l)))

(define (x-end-segment l)
  (x-point (end-segment l)))

(define (y-end-segment l)
  (y-point (end-segment l)))

(define (x-len-segment l)
  (- (x-end-segment l) (x-start-segment l)))

(define (y-len-segment l)
  (- (y-end-segment l) (y-start-segment l)))

(define (length-segment l)
  (sqrt (+ (square (x-len-segment l)) (square (y-len-segment l)))))

(define (vertical? l)
  (let ((x1 (x-start-segment l))
        (x2 (x-end-segment l)))
    (if (and (rational? x1) (rational? x2))
      (= x1 x2)
      (is-close? x1 x2))))

(define (slope-segment l)
  (if (vertical? l)
    +inf.0
    (/ (y-len-segment l) (x-len-segment l))))

(define (parallel? l1 l2)
  (cond
    ((vertical? l1) (vertical? l2))
    ((vertical? l2) (vertical? l1))
    (else (is-close? (slope-segment l1) (slope-segment l2)))))

(define (perpendicular? l1 l2)
  (cond
    ((vertical? l1) (zero? (slope-segment l2)))
    ((vertical? l2) (zero? (slope-segment l1)))
    (else (is-close? (* (slope-segment l1) (slope-segment l2)) -1))))

(define (segment-func f)
  (lambda (l)
    (if (segment? l)
      (f l)
      (error "Argument must be a segment"))))

(define start-segment (segment-func car))

(define end-segment (segment-func cdr))
