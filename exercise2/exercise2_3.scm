(load "lib/math.scm")
(load "lib/geometry.scm")

(define (point-op l)
  (lambda points
    (make-point (apply l (map x-point points)) (apply l (map y-point points)))))

(define point-add (point-op +))

(define point-sub (point-op -))

(define point-mult (point-op *))

(define point-div (point-op /))

(define (point-absop l)
  (lambda (p n)
    (make-point (l (x-point p) n) (l (y-point p) n))))

(define point-absadd (point-absop +))

(define point-abssub (point-absop -))

(define point-absmult (point-absop *))

(define point-absdiv (point-absop /))

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

(define (segments-rectangle r)
  (cdr r))

(define (points-rectangle r)
  (car r))

(define (rectangle? r)
  (if (not (pair? r))
    #f
    (let ((pts (points-rectangle r))
          (rs (segments-rectangle r)))
      (if (not (and (list? pts) (= (length pts) 4)
                (list? rs)
                (= (length rs) 4)))
        #f
        (let ((rs0 (list-ref rs 0))
              (rs1 (list-ref rs 1))
              (rs2 (list-ref rs 2))
              (rs3 (list-ref rs 3)))
          (and (parallel? rs0 rs2)
            (parallel? rs1 rs3)
            (perpendicular? rs0 rs1)))))))

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

(define (print-rectangle r)
  (print-pointlist (points-rectangle r)))

(define (centroid . points)
  (point-absdiv (apply point-add points) (length points)))

(define (radialsort-points . points)
  (let ((c (apply centroid points)))
    (sort points
      (lambda (p1 p2)
        (let ((v1 (point-sub p1 c))
              (v2 (point-sub p2 c)))
          (< (atan (y-point v1) (x-point v1))
            (atan (y-point v2) (x-point v2))))))))

(define (make-rectangle p0 p1 p2 p3)
  (let* ((sorted (radialsort-points p0 p1 p2 p3))
         (s0 (list-ref sorted 0))
         (s1 (list-ref sorted 1))
         (s2 (list-ref sorted 2))
         (s3 (list-ref sorted 3))
         (r (cons (list s0 s1 s2 s3) (list (make-segment s0 s1) (make-segment s1 s2) (make-segment s2 s3) (make-segment s3 s0)))))
    (if (rectangle? r)
      r
      (error "Provided points do not make a valid rectangle"))))

(define (perimeter-shape s)
  (define (iter s acc)
    (if (null? s)
      acc
      (iter (cdr s) (+ acc (length-segment (car s))))))

  (iter s 0))

(define (area-shape points)
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

(define (perimeter-rectangle r)
  (perimeter-shape (segments-rectangle r)))

(define (area-rectangle r)
  (area-shape (points-rectangle r)))

;; Alternative rectangle constructors
(define (make-rectangle-corners p1 p2)
  (let ((p3 (make-point (x-point p1) (y-point p2)))
        (p4 (make-point (x-point p2) (y-point p1))))
    (make-rectangle p1 p2 p3 p4)))

(define (make-rectangle-centroid-corner c p1)
  (let* ((p-opp (point-sub (point-absmult c 2) p1))
         (p2 (make-point (x-point p1) (y-point p-opp)))
         (p3 (make-point (x-point p-opp) (y-point p1))))
    (make-rectangle p1 p-opp p2 p3)))

(define (width-rectangle r)
  (length-segment (car (segments-rectangle r))))

(define (height-rectangle r)
  (length-segment (car (cdr (segments-rectangle r)))))

;; Alternative rectangle representation
(define (light-rectangle? r)
  (let ((origin (list-ref r 0))
        (width (list-ref r 1))
        (height (list-ref r 2)))
    (and (point? origin) (number? width) (number? height))))

(define (make-light-rectangle origin width height)
  (let ((r (list origin width height)))
    (if (light-rectangle? r)
      r
      (error "Improperly defined light rectangle"))))

(define (origin-light-rectangle r)
  (car r))

(define (width-light-rectangle r)
  (list-ref r 1))

(define (height-light-rectangle r)
  (list-ref r 2))

;; General methods where data is abstracted away
(define (general-width-rectangle r)
  (cond
    ((rectangle? r) (width-rectangle r))
    ((light-rectangle? r) (width-light-rectangle r))
    (else (error "Provided argument is not a valid rectangle!"))))

(define (general-height-rectangle r)
  (cond
    ((rectangle? r) (height-rectangle r))
    ((light-rectangle? r) (height-light-rectangle r))
    (else (error "Provided argument is not a valid rectangle!"))))

(define (general-area-rectangle r)
  (* (general-width-rectangle r) (general-height-rectangle r)))

(define (general-perimeter-rectangle r)
  (* (+ (general-width-rectangle r) (general-height-rectangle r)) 2))

;; Testing of implementations
(define r1 (make-rectangle (make-point 0 0) (make-point 4 0) (make-point 4 3) (make-point 0 3)))
(define r2 (make-rectangle-corners (make-point 0 0) (make-point 4 3)))
(define r3 (make-rectangle-centroid-corner (make-point 2 1.5) (make-point 0 0)))
(define r4 (make-light-rectangle (make-point 0 0) 4 3))

(newline)
(display "Specialised methods for heavy rectangles.")
(newline)
(display (string-append "(area-rectangle r1) => " (number->string (area-rectangle r1))))
(newline)
(display (string-append "(perimeter-rectangle r1) => " (number->string (perimeter-rectangle r1))))
(newline)
(display (string-append "(area-rectangle r2) => " (number->string (area-rectangle r2))))
(newline)
(display (string-append "(perimeter-rectangle r2) => " (number->string (perimeter-rectangle r2))))
(newline)
(display (string-append "(area-rectangle r3) => " (number->string (area-rectangle r3))))
(newline)
(display (string-append "(perimeter-rectangle r3) => " (number->string (perimeter-rectangle r3))))
(newline)
(newline)
(display "Generalised methods for all described rectangle representations.")
(newline)
(display (string-append "(general-area-rectangle r1) => " (number->string (general-area-rectangle r1))))
(newline)
(display (string-append "(general-perimeter-rectangle r1) => " (number->string (general-perimeter-rectangle r1))))
(newline)
(display (string-append "(general-area-rectangle r2) => " (number->string (general-area-rectangle r2))))
(newline)
(display (string-append "(general-perimeter-rectangle r2) => " (number->string (general-perimeter-rectangle r2))))
(newline)
(display (string-append "(general-area-rectangle r3) => " (number->string (general-area-rectangle r3))))
(newline)
(display (string-append "(general-perimeter-rectangle r3) => " (number->string (general-perimeter-rectangle r3))))
(newline)
(display (string-append "(general-area-rectangle r4) => " (number->string (general-area-rectangle r4))))
(newline)
(display (string-append "(general-perimeter-rectangle r4) => " (number->string (general-perimeter-rectangle r4))))
(newline)
