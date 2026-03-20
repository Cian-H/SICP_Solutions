(define (install-rational-package)
  (register-type 'rational)

  ;;; Define type methods
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make n d)
    (let* ((n-norm (if (negative? d) (- n) n))
           (g (gcd n-norm d)))
      (cons (/ n-norm g) (/ (abs d) g))))

  (define (print x)
    (display (numer x))
    (display "/")
    (display (denom x)))

  (define (add x y)
    (make
      (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
      (* (denom x) (denom y))))

  (define (sub x y)
    (make
      (- (* (numer x) (denom y))
        (* (numer y) (denom x)))
      (* (denom x) (denom y))))

  (define (mul x y)
    (make
      (* (numer x) (numer y))
      (* (denom x) (denom y))))

  (define (div x y)
    (make
      (* (numer x) (denom y))
      (* (denom x) (numer y))))

  (define (equal? x y)
    (=
      (* (numer x) (denom y))
      (* (denom x) (numer y))))

  (define (rational-zero? x)
    (zero? (numer x)))

  (define (tag x) (type-wrap 'rational x))

  ;;; Register type methods
  (put 'make 'rational
    (lambda (n d) (tag (make n d))))

  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'add 'rational 'rational
    (lambda (x y) (tag (add x y))))
  (put 'sub 'rational 'rational
    (lambda (x y) (tag (sub x y))))
  (put 'mul 'rational 'rational
    (lambda (x y) (tag (mul x y))))
  (put 'div 'rational 'rational
    (lambda (x y) (tag (div x y))))
  (put 'equal? 'rational 'rational equal?)
  (put 'zero? 'rational rational-zero?)

  ;;; Add type coercions
  (define (number->rational n)
    (tag (make (type-unwrap n) 1)))

  (define (rational->number n)
    (let ((unwrapped-n (type-unwrap n)))
      (/ (numer unwrapped-n) (denom unwrapped-n))))

  (put-coercion 'number 'rational number->rational)
  (put-coercion 'rational 'number rational->number)

  'ok)
