(define (segment? s)
  (and (pair? s) (vect? (car s)) (vect? (cdr s))))

(define (_notsegment-error s) (_monotype-error s "segment"))

(define (guarded-segment s func-name)
  (if (segment? s) s (_notsegment-error func-name)))

(define (segment-list? l)
  (define (rec l)
    (if (null? l) #t
      (let ((lh (car l))
            (lt (cdr l)))
        (if (segment? lh)
          (rec lt)
          #f)))))

(define (_notsegment-list-error s) (_monotype-error s "segment-list"))

(define (guarded-segment-list l func-name)
  (if (segment-list? s) l (_notsegment-list-error func-name)))

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

(define (segments->painter segment-list)
  (let ((l (guarded-segment-list segment-list)))
    (lambda (frame)
      (let ((frame-map (frame-coord-map frame)))
        (for-each
          (lambda (segment)
            (draw-line
              (frame-map (start-segment segment))
              (frame-map (end-segment segment))))
          l)))))
