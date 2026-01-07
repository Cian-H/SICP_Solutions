(define make-point cons)

(define x-point car)

(define y-point cdr)

(define point? pair?)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-pointlist points)
  (display "(")
  (if (not (null? points))
    (begin
      (print-point (car points))
      (for-each (lambda (p)
                 (display ",")
                 (print-point p))
        (cdr points))))
  (display ")"))

(define (centroid-point . points)
  (absdiv-point (apply add-point points) (length points)))

(define (radialsort-points . points)
  (let ((c (apply centroid-point points)))
    (sort points
      (lambda (p1 p2)
        (let ((v1 (sub-point p1 c))
              (v2 (sub-point p2 c)))
          (< (atan (y-point v1) (x-point v1))
            (atan (y-point v2) (x-point v2))))))))

(define (point-op l)
  (lambda points
    (make-point (apply l (map x-point points)) (apply l (map y-point points)))))

(define add-point (point-op +))

(define sub-point (point-op -))

(define mult-point (point-op *))

(define div-point (point-op /))

(define eq-point (point-op =))

(define (point-absop l)
  (lambda (p n)
    (make-point (l (x-point p) n) (l (y-point p) n))))

(define absadd-point (point-absop +))

(define abssub-point (point-absop -))

(define absmult-point (point-absop *))

(define absdiv-point (point-absop /))
