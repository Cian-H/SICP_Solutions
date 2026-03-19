(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (let* ((n-norm (if (negative? d) (- n) n))
           (g (gcd n-norm d)))
      (cons (/ n-norm g) (/ (abs d) g))))

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

  (define (tag x) (type-wrap 'rational x))

  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))

  (put 'add 'rational 'rational
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub 'rational 'rational
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul 'rational 'rational
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div 'rational 'rational
    (lambda (x y) (tag (div-rat x y))))
  'ok)
