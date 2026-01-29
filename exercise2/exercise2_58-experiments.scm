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
    '(((x * y) ** 2) + 4)))

; Attempt 2
;;; The only way I see around this is to use `takewhile` and `memq` to split at operations to the
;;; left and right
(define (deriv expr var)
  (cond ((number? expr) 0)
    ((variable? expr) (if (same-variable? expr var) 1 0))
    ((sub? expr) (make-sub
                  (deriv (minuend expr) var)
                  (deriv (subtrahend expr) var)))
    ((sum? expr) (make-sum
                  (deriv (addend expr) var)
                  (deriv (augend expr) var)))
    ((div? expr) (make-div
                  (make-sub
                    (make-product (divisor expr) (deriv (dividend expr) var))
                    (make-product (dividend expr) (deriv (divisor expr) var)))
                  (make-exponentiation (divisor expr) 2)))
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

; Needed to deal with singleton variables intside brackets
(define (simplify expr)
  (if (null? (cdr expr)) (car expr) expr))

(define (create-op-predicate s) (lambda (x) (and (pair? x) (memq s x))))

(define (get-prefix s expr)
  (simplify (take-while (lambda (x) (not (eq? x s))) expr)))

(define (get-suffix s expr) (cdr (memq s expr)))

;;; Basic predicates and selectors
(define sub? (create-op-predicate '-))
(define (minuend expr) (get-prefix '- expr))
(define (subtrahend expr) (simplify (get-suffix '- expr)))

(define sum? (create-op-predicate '+))
(define (addend expr) (get-prefix '+ expr))
(define (augend expr) (simplify (get-suffix '+ expr)))

(define div? (create-op-predicate '/))
(define (dividend expr) (get-prefix '/ expr))
(define (divisor expr) (simplify (get-suffix '/ expr)))

(define product? (create-op-predicate '*))
(define (multiplier expr) (get-prefix '* expr))
(define (multiplicand expr) (simplify (get-suffix '* expr)))

(define exponentiation? (create-op-predicate '**))
(define (base expr) (get-prefix '** expr))
(define (exponent expr) (car (get-suffix '** expr)))

;;; Could be considered over-engineering for this stage, but decided I wanted to try and design
;;; an abstraction here that would generalise to all algebraic operations
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

(define (create-make-expr-symmetric op s absorber identity inverse extra-properties)
  (create-make-expr op s absorber absorber identity identity inverse extra-properties))

;;; Operation inversions
(define (inverse-sum s)
  (cond
    ((number? s) (- s))
    ((and (pair? s) (eq? (cadr s) '-) (equal? (car s) 0)) (caddr s)) ;; Catch double negatives early here for simplicity
    (else (list 0 '- s))))

(define (inverse-sub s) s)

(define (inverse-product s)
  (if (number? s) (/ 1 s) (list 1 '/ s)))

(define (inverse-div s) s)

;;; Properties
(define (sub-properties x y z)
  (cond
    ((and (=number? x 0) (pair? y) (eq? (cadr y) '-) (=number? (car y) 0)) (caddr y)) ;; -(-n) = n
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
(define make-product (create-make-expr-symmetric * '* 0 1 inverse-product #f))
(define make-div (create-make-expr / '/ 0 #f #f 1 inverse-div #f))
(define make-exponentiation (create-make-expr expt '** #f #f #f 1 #f exponentiation-properties))

; ~~~ Implementation Testing ~~~
(run-tests "Attempt 2" (lambda (expr) (deriv expr 'x))
  (list
    '(x + 3)
    '(x * y)
    '(x * y * (x + 3))
    '(x + y + 3)
    '(x * y * 3)
    '(x * y * (x + y + 3))
    '((x * y) ** 2 + 4)))

(run-tests "Attempt 2 - Stress Testing" (lambda (expr) (deriv expr 'x))
  (list
    ;;; 1. The Basics (Sanity Check)
    '(x + 3)
    '(x * y)
    ;;; 2. Polynomials (Tests Sum + Power Rule combination)
    '((x ** 3) + (3 * (x ** 2)) + (2 * x) + 1)
    ;;; 3. The Chain Rule (Power of a function)
    ;;; Requires: d/dx(u^n) = n*u^(n-1) * du/dx
    '((x + 3) ** 5)
    '(((x ** 2) + 1) ** 3)
    ;;; 4. The Quotient Rule (Division)
    ;;; Requires: d/dx(u/v) = (v*du/dx - u*dv/dx) / v^2
    '((x + 1) / (x - 1))
    '(1 / x) ; Reciprocal rule
    '(1 / (x ** 2)) ; Reciprocal of power
    ;;; 5. Mixed Operations (Product + Sum + Div)
    ;;; Great for testing operator precedence assumptions
    '((x * y) / (x + y))
    ;;; 6. Subtraction & Negation
    ;;; Tests if (x - x) simplifies to 0 and signs are handled
    '(x - (3 * x))
    '((x * y) - (x / y))
    ;;; 7. Deep Nesting / Torture Test
    ;;; Tests recursion depth and accumulator logic
    '(x * (x + (x * (x + 1))))
    '((x ** 2) * (y ** 2) * ((x + 1) ** 2))))
