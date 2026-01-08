(define (print-polygon s)
  (print-pointlist (points-polygon s)))

(define (make-polygon-points . points)
  (let* ((sorted (apply radialsort-points points))
         (segments (map make-segment sorted (append (cdr sorted) (list (car sorted)))))
         (poly (cons sorted segments)))
    (if (polygon? poly)
      poly
      (error "Provided points do not make a valid polygon"))))

(define (segments-polygon s)
  (cdr s))

(define (points-polygon s)
  (car s))

(define (closed? s)
  (define (iter sl acc)
    (let ((sh (car sl))
          (st (cdr sl)))
      (if (null? st)
        acc
        (iter st (and acc (eq-point (end-segment sh) (start-segment (car st))))))))

  (let* ((ss (segments-polygon s))
         (wrapped (append ss (list (car ss)))))
    (iter wrapped #t)))

(define (polygon? s)
  (define (all-points? st acc)
    (if (null? st)
      acc
      (all-points? (cdr st) (and acc (point? (car st))))))

  (define (all-segments? st acc)
    (if (null? st)
      acc
      (all-segments? (cdr st) (and acc (segment? (car st))))))

  (if (not (pair? p))
    #f
    (let* ((ps (points-polygon s))
           (ss (segments-polygon s))
           (ss0 (list-ref ss 0))
           (ss1 (list-ref ss 1))
           (ss2 (list-ref ss 2))
           (ss3 (list-ref ss 3)))
      (and (list? ps)
        (list? ss)
        (= (length ps) (length ss))
        (all-points? ps #t)
        (all-segments? ss #t)
        (closed? s)))))

(define (perimeter-polygon s)
  (define (iter s acc)
    (if (null? s)
      acc
      (iter (cdr s) (+ acc (length-segment (car s))))))

  (iter s 0))

(define (area-polygon points)
  (let ((closed (append points (list (car points)))))
    (define (shoelace-iter pts acc)
      (if (null? (cdr pts))
        acc
        (let ((p1 (car pts))
              (p2 (cadr pts)))
          (shoelace-iter (cdr pts)
            (+ acc (- (* (x-point p1) (y-point p2))
                    (* (x-point p2) (y-point p1))))))))
    (/ (abs (shoelace-iter closed 0)) 2)))
