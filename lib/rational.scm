(define (make-rat n d)
  (let ((n_norm (if (negative? d) (- n) n))
        (g (gcd n_norm d)))
    (cons (/ n_norm g) (/ (abs d) g))))

(define numer car)

(define denom cdr)

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat
    (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
    (- (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat
    (* (numer x) (denom y))
    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (=
    (* (numer x) (denom y))
    (* (denom x) (numer y))))
