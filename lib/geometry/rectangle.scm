(define (print-rectangle r)
  (print-polygon r))

(define (segments-rectangle r)
  (segments-polygon r))

(define (points-rectangle r)
  (points-polygon r))

(define (rectangle? r)
  (define (all-points? rt acc)
    (if (null? rt)
      acc
      (all-points? (cdr rt) (and acc (point? (car rt))))))

  (define (all-segments? rt acc)
    (if (null? rt)
      acc
      (all-segments? (cdr rt) (and acc (segment? (car rt))))))

  (let* ((rs (segments-rectangle r))
         (rs0 (list-ref rs 0))
         (rs1 (list-ref rs 1))
         (rs2 (list-ref rs 2))
         (rs3 (list-ref rs 3)))
    (and (polygon? r)
      (= (length (points-rectangle r)) 4)
      (parallel? rs0 rs2)
      (parallel? rs1 rs3)
      (perpendicular? rs0 rs1))))

(define (make-rectangle-points p0 p1 p2 p3)
  (let ((r (make-polygon-points p0 p1 p2 p3)))
    (if (rectangle? r)
      r
      (error "Provided points do not make a valid rectangle"))))

(define (make-rectangle-corners p1 p2)
  (let ((p3 (make-point (x-point p1) (y-point p2)))
        (p4 (make-point (x-point p2) (y-point p1))))
    (make-rectangle-points p1 p2 p3 p4)))

(define (make-rectangle-centroid-corner c p1)
  (let* ((p-opp (sub-point (absmult-point c 2) p1))
         (p2 (make-point (x-point p1) (y-point p-opp)))
         (p3 (make-point (x-point p-opp) (y-point p1))))
    (make-rectangle-points p1 p-opp p2 p3)))

(define (perimeter-rectangle r)
  (perimeter-polygon (segments-rectangle r)))

(define (area-rectangle r)
  (area-polygon (points-rectangle r)))
