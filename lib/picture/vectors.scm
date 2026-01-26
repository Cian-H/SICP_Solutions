(load "lib/functools.scm")

;;; I could've just done `(define make-vect cons)` but being defensive here should make it possible
;;; to avoid brittleness while giving us a single place to enforce a valid construction if types
;; change later
(define (make-vect x y)
  (if (and (valid-coordinate? x) (valid-coordinate? y))
    (cons x y)
    (_monotype-error "valid-coordiate")))

;;; From experience so far: being defensive in scheme seems to benefit from creating a typechecking
;;; Predicate up-front. Later, if i switch to tagged data i can just change the predicate to get
;;; an even more robust check without refactoring everything.
(define (vect? x)
  (and (pair? x)
    (valid-coordinate? (car x))
    (valid-coordinate? (cdr x)))) ; If it looks/quacks like a duck, lets treat it like one...

(define (_notvect-error s) (_monotype-error s "vect"))

(define (guarded-vect v func-name)
  (if (vect? v) v (_notvect-error func-name)))

;;; Let's also create similar tools for lists of vects
(define (vect-list? l)
  (define (rec l)
    (if (null? l) #t
      (let ((lh (car l))
            (lt (cdr l)))
        (if (vect? lh)
          (rec lt)
          #f))))

  (if (or (null? l) (not (list? l))) #f (rec l)))

(define (_notvectlist-error s)
  (error (string-append s " only supports `vect-list` arguments")))

(define (guarded-vect-list v func-name)
  (if (vect-list? v) v (_notvectlist-error func-name)))

;;; Now that we have guards, we can just add guard clauses here to enforce correctness.
;;; Assuming we will *always* be accessing these via these interfaces, we should be able to
;;; contain our guard clauses to these procedures except in edge-cases. This will allow us to
;;; maintain performance while (mostly) enforcing correctness at the I/O for our problem domain.
(define (xcor-vect v)
  (car (guarded-vect v "xcor-vect")))

(define (ycor-vect v)
  (cdr (guarded-vect v "ycor-vect")))

;;; Display procedures are handy for debugging too
(define (vect->string v)
  (string-append "<" (coordinate->string (xcor-vect v)) "," (coordinate->string (ycor-vect v)) ">"))

(define (display-vect v) (display (vect->string v)))

(define (vect-list->string l)
  (map vect->string l))

(define (display-vect-list v) (display (vect-list->string l)))

;;; The guard exits early and folds efficiently, so 2 passes applyging `xcor-vect-list` and mapping
;;; `car` should be more efficient than 1 pass mapping `xcor-vect`.
(define (xcor-vect-list . vectors)
  (map car (guarded-vect-list vectors "xcor-vect-list")))

(define (ycor-vect-list . vectors)
  (map cdr (guarded-vect-list vectors "ycor-vect-list")))

(define (elementwise-vector-op op)
  (lambda (v1 v2)
    (make-vect
      (op (xcor-vect v1) (xcor-vect v2))
      (op (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (elementwise-vector-op +))
(define sub-vect (elementwise-vector-op -))
(define mul-vect-elementwise (elementwise-vector-op *))
(define div-vect-elementwise (elementwise-vector-op /))

;;; Note: I *could* implement this by mapping the primitives above but that would hurt performance
(define (elementwise-vector-list-op op identity)
  (lambda vectors
    (let* ((xs (map xcor-vect vectors)) ;; No need for guarding list, as elements are guarded here
           (ys (map ycor-vect vectors)))
      (make-vect
        (fold-left op identity xs)
        (fold-left op identity ys)))))

(define add-vect-list (elementwise-vector-list-op + 0))
(define sub-vect-list (elementwise-vector-list-op - 0))
(define mul-vect-elementwise-list (elementwise-vector-list-op * 1))
(define div-vect-elementwise-list (elementwise-vector-list-op / 1))

(define (scalar-vector-op op)
  (lambda (v1 v2)
    (let ((v1-isvect (vect? v1))
          (v2-isvect (vect? v2)))
      (if (eq? v1-isvect v2-isvect)
        (error "A scalar vector operation must be provided a scalar AND a vector as arguments")
        (let ((v1x (if v1-isvect (xcor-vect v1) v1))
              (v1y (if v1-isvect (ycor-vect v1) v1))
              (v2x (if v2-isvect (xcor-vect v2) v2))
              (v2y (if v2-isvect (ycor-vect v2) v2)))
          (make-vect (op v1x v2x) (op v1y v2y)))))))

(define sadd-vect (scalar-vector-op +))
(define ssub-vect (scalar-vector-op -))
(define smul-vect (scalar-vector-op *))
(define sdiv-vect (scalar-vector-op /))

;;; Now, scale-vect is just an alias for a more general operation
(define scale-vect smul-vect)

;;; Again, this *should* be more perfomant than mapping the procedures above
(define (scalar-vector-apply-op op)
  (lambda (s vectors)
    (map (lambda (x)
          (make-vect
            (op s (xcor-vect x)) ;; No need to guard vector as elements are guarded here
            (op s (ycor-vect x))))
      vectors)))

(define sadd-vect-apply (scalar-vector-apply-op +))
(define ssub-vect-apply (scalar-vector-apply-op -))
(define smul-vect-apply (scalar-vector-apply-op *))
(define sdiv-vect-apply (scalar-vector-apply-op /))
