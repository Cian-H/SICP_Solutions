(define (frame? f)
  (and (vec-list? f) (= (length f 3))))

(define (_notframe-error s) (_monotype-error s "frame"))

(define (guarded-frame f func-name)
  (if (vect? f) f (_notframe-error func-name)))

(define (make-frame origin edge1 edge2)
  (define (guard v) (guarded-vect v "make-frame"))
  (let ((o (guard origin))
        (e1 (guard edge1))
        (e2 (guard edge2)))
    (list o e1 e2)))

(define (origin-frame f)
  (guarded-vect (car f) "origin-frame"))

(define (edge1-frame f)
  (guarded-vect (cadr f) "edge1-frame"))

(define (edge2-frame f)
  (guarded-vect (caddr f) "edge2-frame"))

(define (frame->string f)
  (let ((o (vect->string (origin-frame f)))
        (e1 (vect->string (edge1-frame f)))
        (e2 (vect->string (edge2-frame f))))
    (string-append "{.->" o "_" e1 "|" e2 "}")))

(define (display-frame f) (display (frame->string f)))

(define (frame-coord-map f)
  (let ((frame (guarded-frame f)))
    (lambda (v)
      (add-vect
        (origin-frame frame)
        (add-vect
          (scale-vect (xcor-vect v) (edge1-frame frame))
          (scale-vect (ycor-vect v) (edge2-frame frame)))))))
