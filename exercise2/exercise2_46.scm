;;; This one might seem a bit over-engineered but i want to build a convenient base for later
;;; experiments with the picture language in my `lib/picture.scm` library. So, instead of doing
;;; the minimal version here and refactoring later I figured I'd take a "right first time"
;;; approach, since i know what kinds of felexibility i generally want in my functional programming
;;; DSLs.

;;; A lot of the errors in here will be these simple, single type typecheck fails
(define (_monotype-error s t)
  (error (string-append s " only supports `" t "` arguments")))

;;; I could've just done `(define make-vect cons)` but being defensive here should make this less
;;; brittle later.
(define (make-vect x y)
  (if (and (number? x) (number? y)) (cons x y) (_monotype-error "number")))

;;; From experience so far: being defensive in scheme seems to benefit from creating a typechecking
;;; Predicate up-front. Later, if i switch to tagged data i can just change the predicate to get
;;; an even more robust check without refactoring everything
(define (vect? x)
  (and (pair? x) (number? (car x)) (number? (cdr x)))) ; If it looks/quacks like a duck...

(define (_notvect-error s) (_monotype-error s "vect"))

(define (guarded-vect v func-name)
  (if (vect? v) v (_notvect-error func-name)))

;;; Now that we have guards, we can just add guard clauses here to enforce correctness.
;;; Assuming we will *always* be accessing these via these interfaces, we should be able to
;;; contain our guard clauses to these procedures except in edge-cases. This will allow us to
;;; maintain performance while (mostly) enforcing correctness at the I/O for our problem domain.
(define (xcor-vect v)
  (car (guarded-vect v "xcor-vect")))

(define (ycor-vect v)
  (cdr (guarded-vect v "ycor-vect")))

(define (elementwise-vector-op op)
  (lambda (v1 v2)
    (make-vect
      (op (xcor-vect v1) (xcor-vect v2))
      (op (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (elementwise-vector-op +))
(define sub-vect (elementwise-vector-op -))

;;; I want this to also support non-commutative operations. If performance is a bottleneck later
;;; I can add a basic strategy selector to create a commutative and non-commutative path
(define (scalar-vector-op op)
  (lambda (v1 v2)
    (let ((v1-isvect (vect? v1))
          (v2-isvect (vect? v2)))
      (if (eq? v1-isvect v2-isvect)
        (error "A scalar vector operation must be provided a scalar and a vector as arguments")
        (let ((v1x (if v1-isvect (xcor-vect v1) v1))
              (v1y (if v1-isvect (ycor-vect v1) v1))
              (v2x (if v2-isvect (xcor-vect v2) v2))
              (v2y (if v2-isvect (ycor-vect v2) v2)))
          (make-vect (op v1x v2x) (op v1y v2y)))))))

(define smul-vect (scalar-vector-op *))
(define scale-vect smul-vect) ; With these abstractions: scale-vect is just an alias for a more general operation
