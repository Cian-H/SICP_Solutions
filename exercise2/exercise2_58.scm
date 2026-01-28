; An extra feature i added in my original exercise 2.56 implementation.
; Some of the functions that already exist here will make the infix modifications easier to do.
(load "lib/functools.scm")
(define algebraic-ops (list '+ '- '* '/ '**))

(define (algebraic-op? x)
  (if (memq x algebraic-ops) #t #f))

(define (filter-vars l)
  (unique (filter
           (lambda (x) (and (variable? x) (not (algebraic-op? x))))
           (enumerate-tree l))))

; Aggregated parts of the original implementations
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (variadic-arg-selector s)
  (if (algebraic-op? s)
    (lambda (l)
      (let ((lt (cddr l))
            (lth (caddr l))
            (ltt (cdddr l)))
        (if (null? ltt) lth (cons s lt))))
    (error "Cannot create variadic argument selector for a non algebraic symbol")))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define augend (variadic-arg-selector '+))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier s) (cadr s))
(define multiplicand (variadic-arg-selector '*))

(define (=number? expr num) (and (number? expr) (= expr num)))

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2))))

(define (expr->lambda expr)
  (eval (list 'lambda (filter-vars expr) expr)
    (interaction-environment)))

(define (make-exponentiation b e)
  (cond
    ((=number? e 0) 1)
    ((=number? b 0) 0)
    ((=number? e 1) b)
    ((and (number? b) (number? e)) (expt b e))
    (else (list '** b e))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (deriv expr var)
  (cond ((number? expr) 0)
    ((variable? expr) (if (same-variable? expr var) 1 0))
    ((sum? expr) (make-sum (deriv (addend expr) var)
                  (deriv (augend expr) var)))
    ((product? expr) (make-sum
                      (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
                      (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
    ((exponentiation? expr) (make-product
                             (make-product
                               (exponent expr)
                               (make-exponentiation
                                 (base expr)
                                 (make-sum (exponent expr) -1)))
                             (deriv (base expr) var)))
    (else (error "Unknown expression type: DERIV"))))

; ~~~ Define testing procedure ~~~
(define (run-tests label op expr-list)
  (newline)
  (display (string-append "~~~ " label " ~~~"))
  (newline)
  (for-each (lambda (expr)
             (display expr)
             (display " => ")
             (display (op expr))
             (newline))
    expr-list)
  (newline))

; ~~~ Ground truth for testing ~~~
(run-tests "Original Implementation" (lambda (expr) (deriv expr 'x))
  (list
    '(+ x 3)
    '(* x y)
    '(* (* x y) (+ x 3))
    '(+ x y 3)
    '(* x y 3)
    '(* x y (+ x y 3))
    '(+ (** (* x y) 2) 4)))

; ~~~ Exercise Start ~~~
; Attempt 1
(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list m1 '* m2))))

(define (make-exponentiation b e)
  (cond
    ((=number? e 0) 1)
    ((=number? b 0) 0)
    ((=number? e 1) b)
    ((and (number? b) (number? e)) (expt b e))
    (else (list b '** e))))

(define (infix-operator-predicate-factory s)
  (lambda (x) (and (pair? x) (eq? (cadr x) s))))

(define (infix-first-arg-selector expr)
  (car expr))

(define (infix-variadic-arg-selector s)
  (if (algebraic-op? s)
    (lambda (l)
      (let ((lt (cddr l))
            (lth (caddr l))
            (ltt (cdddr l)))
        (if (null? ltt) lth (cons lth (cons s lt)))))
    (error "Cannot create variadic argument selector for a non algebraic symbol")))

(define sum? (infix-operator-predicate-factory '+))
(define addend infix-first-arg-selector)
(define augend (infix-variadic-arg-selector '+))

(define product? (infix-operator-predicate-factory '*))
(define multiplier infix-first-arg-selector)
(define multiplicand (infix-variadic-arg-selector '*))

(define exponentiation? (infix-operator-predicate-factory '**))
(define base infix-first-arg-selector)
(define (exponent s) (caddr s))

; ~~~ Implementation Testing ~~~
(run-tests "Attempt 1 (only works with brackets)" (lambda (expr) (deriv expr 'x))
  (list
    '(x + 3)
    '(x * y)
    '((x * y) * (x + 3))
    '((x + y) + 3)
    '((x * y) + 3)
    '((x * y) * ((x + y) + 3))
    '((x * (y ** 2)) + 4)))

; Attempt 2
;;; The only way I see around this is to use `takewhile` and `memq` to split at operations to the
;;; left and right

; Needed to deal with singleton variables intside brackets
(define (simplify exp)
  (if (null? (cdr exp)) (car exp) exp))

(define (sum? x)
  (and (pair? x) (memq '+ x)))

(define (product? x)
  (and (pair? x) (not (sum? x)) (memq '* x)))

(define (get-prefix sym list)
  (simplify (take-while (lambda (x) (not (eq? x sym))) list)))

(define (addend s)
  (get-prefix '+ s))

(define (augend s)
  (simplify (cdr (memq '+ s))))

(define (multiplier s)
  (get-prefix '* s))

(define (multiplicand s)
  (simplify (cdr (memq '* s))))

; ~~~ Implementation Testing ~~~
(run-tests "Attempt 2" (lambda (expr) (deriv expr 'x))
  (list
    '(x + 3)
    '(x * y)
    '(x * y * (x + 3))
    '(x + y + 3)
    '(x * y + 3)
    '(x * y * (x + y + 3))
    '(x * y ** 2 + 4)))
