(define (create-make-expr op s
         left-absorber
         right-absorber
         left-identity
         right-identity
         inverse
         extra-properties)

  (define (apply-extra-properties x y z)
    (if extra-properties (extra-properties x y z) z))

  (lambda (x y)
    (apply-extra-properties x y
      (cond
        ((and left-absorber (number? x) (=number? x left-absorber)) left-absorber)
        ((and right-absorber (number? y) (=number? y right-absorber)) right-absorber)
        ((and left-identity (number? x) (=number? x left-identity)) y)
        ((and right-identity (number? y) (=number? y right-identity)) x)
        ((and inverse right-identity (equal? (inverse x) y)) right-identity)
        ((and inverse left-identity (equal? (inverse y) x)) left-identity)
        ((and (number? x) (number? y)) (op x y))
        (else (list x s y)))))) ; Important: infix here

(define (type-precedent x)
  (cond ((number? x) 1)
    ((symbol? x) 2)
    ((pair? x) 3)
    (else 4)))

(define (expr<? a b)
  (let ((score-a (type-precedent a))
        (score-b (type-precedent b)))
    (cond
      ((< score-a score-b) #t)
      ((> score-a score-b) #f)
      ((number? a) (< a b))
      ((symbol? a) (string<? (symbol->string a) (symbol->string b)))
      ((pair? a) (let ((op-a (lowest-precedence-op a))
                       (op-b (lowest-precedence-op b)))
                  (cond
                    ((not (eq? op-a op-b)) (< (op-precedence op-a) (op-precedence op-b)))
                    ((not (equal? (get-prefix op-a a) (get-prefix op-b b))) (expr<? (get-prefix op-a a) (get-prefix op-b b)))
                    (else (expr<? (get-suffix op-a a) (get-suffix op-b b))))))
      (else #f))))

(define (create-make-expr-symmetric op s absorber identity inverse extra-properties)
  (let ((maker-std (create-make-expr op s absorber absorber identity identity inverse extra-properties)))
    (lambda (x y)
      (if (expr<? y x)
        (maker-std y x)
        (maker-std x y)))))

;;; Properties
(define (sub-properties x y z)
  (cond
    ((and (=number? x 0) (pair? y) (eq? (cadr y) '-) (=number? (car y) 0)) (caddr y)) ; -(-n) -> n
    ((and (pair? y) (eq? (cadr y) '-) (=number? (car y) 0)) (make-sum x (caddr y))) ; x - (0 - y) -> x + y
    ((and (pair? x) (eq? (cadr x) '-) (=number? (car x) 0)) (make-sub 0 (make-sum (caddr x) y))) ; extracting negative
    (else z)))

(define (product-properties x y z)
  (cond
    ((and (pair? y) (div? y) (=number? (dividend y) 1)) (make-div x (divisor y))) ; x * (1 / y) -> x / y
    ((and (pair? x) (div? x) (=number? (dividend x) 1)) (make-div y (divisor x))) ; y * (1 / x) -> y / x
    (else z)))

(define (log-properties b val z)
  (cond
    ((=number? val 1) 0) ; log(b, 1) = 0
    ((equal? b val) 1) ; log(b, b) = 1
    ((exponentiation? val) (make-product (exponent val) (make-log b (base val)))) ; ln(e^x) -> x * ln(e) -> x
    ((and (number? b) (number? val)) (/ (log val) (log b))) ; Reduce if both numbers
    (else z)))

(define (exponentiation-properties x y z)
  (cond
    ((=number? x 1) 1) ;; 1^y = 1
    ((=number? y 0) 1) ;; x^0 = 1
    ((=number? x 0) 0) ;; 0^y = 0
    (else z)))

;;; Define expression makers
(define make-sum (create-make-expr-symmetric + '+ #f 0 inverse-sum #f))
(define make-sub (create-make-expr - '- #f #f #f 0 inverse-sub sub-properties))
(define make-product (create-make-expr-symmetric * '* 0 1 inverse-product product-properties))
(define make-div (create-make-expr / '/ 0 #f #f 1 inverse-div #f))
(define make-log (create-make-expr log '// #f #f #f #f #f log-properties))
(define (make-ln x) (if (eq? x 'e) 1 (make-log 'e x)))
(define make-exponentiation (create-make-expr expt '** #f #f #f 1 #f exponentiation-properties))
