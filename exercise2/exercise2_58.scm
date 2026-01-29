;;; I do understand this is over-engineered, but i was really enhoying the challenge of creating
;;; A fully functional symbolic algebra engine with working derivative calculus
(load "lib/functools.scm")

(define (simplify expr)
  (if (null? (cdr expr)) (car expr) expr))

;;; Expression Simplifier
(define (simplify-sum-associative a b)
  (cond
    [(and (number? a) (pair? b) (sum? b) (number? (addend b))
        (make-sum (+ a (addend b)) (augend b)))]
    [(and (number? b) (pair? a) (sum? a) (number? (augend a))
        (make-sum (addend a) (+ b (augend a))))]
    [else (make-sum a b)]))

(define (simplify-product-associative a b)
  (cond
    [(and (number? a) (pair? b) (product? b) (number? (multiplier b))
        (make-product (* a (multiplier b)) (multiplicand b)))]
    [(and (number? b) (pair? a) (product? a) (number? (multiplicand a))
        (make-product (multiplier a) (* b (multiplicand a))))]
    [else (make-product a b)]))

(define (simplify expr)
  (cond
    [(null? expr) expr]
    [(number? expr) expr]
    [(symbol? expr) expr]
    [(null? (cdr expr)) (simplify (car expr))]
    [(and (pair? expr) (pair? (cdr expr))
        (let* ((op (cadr expr))
               (lhs (simplify (car expr)))
               (rhs (simplify (caddr expr))))
          (cond
            [(eq? op '+) (simplify-sum-associative lhs rhs)]
            [(eq? op '*) (simplify-product-associative lhs rhs)]
            [(eq? op '-) (make-sub lhs rhs)]
            [(eq? op '/) (make-div lhs rhs)]
            [(eq? op '**) (make-exponentiation lhs rhs)]
            [(eq? op '//) (make-log lhs rhs)]
            [else (list lhs op rhs)])))]

    ;; 3. Fallback for other lists
    [else (map simplify expr)]))

; Fundamental variable handling predicates
(define constants-values
  (let* ((pi (acos -1.0))
         (e (exp 1.0))
         (sqrt2 (sqrt 2.0)))
    (list
      (list 'pi pi)
      (list 'tau (* 2 pi))
      (list 'deg-to-rad (/ pi 180.0))
      (list 'e e)
      (list 'log2e (/ 1 (log 2.0)))
      (list 'log10e (/ 1 (log 10.0)))
      (list 'sqrt2 sqrt2)
      (list 'sqrt1_2 (/ 1.0 sqrt2))
      (list 'phi (/ (+ 1.0 (sqrt 5.0)) 2.0))
      (list 'gamma 0.5772156649015329))))
(define symbolic-constants (map car constants-values))

(define (constant->value s)
  (define (iter l)
    (let ((lh (car l))
          (lt (cdr l)))
      (cond
        ((null? lh) (error (string-concat "Requested constant not found " (symbol->string s))))
        ((eq? s (car lh)) (simplify (cdr lh)))
        (else (iter lt)))))
  (iter constants-values))

(define (evaluate-constants expr)
  (cond
    ((constant-symbol? expr) (constant->value expr))
    ((pair? expr) (map evaluate-constants expr))
    (else expr)))

(define (constant-symbol? x)
  (and (symbol? x) (memq x symbolic-constants)))

(define (variable? x)
  (and (symbol? x) (not (constant-symbol? x))))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expr num) (and (number? expr) (= expr num)))

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

(define log? (create-op-predicate '//))
(define (log-base expr) (get-prefix '// expr))
(define (log-val expr) (simplify (get-suffix '// expr)))

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
    ((and (=number? x 0) (pair? y) (eq? (cadr y) '-) (=number? (car y) 0)) (caddr y)) ; -(-n) -> n
    ((and (pair? y) (eq? (cadr y) '-) (=number? (car y) 0)) (make-sum x (caddr y))) ; x - (0 - y) -> x + y
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

;;; Exponentiation Derivative rules
(define (deriv-exponential-power-rule u v var)
  (make-product
    (make-product v (make-exponentiation u (- v 1)))
    (deriv u var)))

(define (deriv-exponential-exponential-rule expr v var)
  (make-product expr (deriv v var)))

(define (deriv-exponential-constbase-rule expr u v var)
  (make-product
    (make-product expr (make-ln u))
    (deriv v var)))

(define (deriv-exponential-general u v var)
  (make-product
    (make-exponentiation u v)
    (make-sum
      (make-product (deriv v var) (make-ln u))
      (make-product v (make-div (deriv u var) u)))))

;;; Main derivative procedure
(define (deriv expr var)
  (cond
    ((null? expr) expr)
    ((or (number? expr) (constant-symbol? expr)) 0)
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
    ((log? expr) (let ((b (log-base expr))
                       (v (log-val expr)))
                  (cond
                    [(exponentiation? v) (deriv (make-product (exponent v) (make-log b (base v))) var)]
                    [(or (number? b) (constant-symbol? b)
                        ; Optimization for constant base: v' / (v * ln(b))
                        (make-div (deriv v var) (make-product v (make-ln b))))]
                    ; General Rule via Change of Base: d/dx(ln(v)/ln(b))
                    [else (deriv (make-div (make-ln v) (make-ln b)) var)])))
    ((exponentiation? expr) (let ((u (base expr))
                                  (v (exponent expr)))
                             (cond
                               ((number? v) (deriv-exponential-power-rule u v var))
                               ((eq? u 'e) (deriv-exponential-exponential-rule expr v var))
                               ((constant-symbol? u) (deriv-exponential-constbase-rule expr u v var))
                               (else (deriv-exponential-general u v var)))))
    (else (error "Unknown expression type: DERIV"))))

; ~~~ Implementation Testing ~~~
; Define testing procedure
(define (run-tests label op expr-input-target-list)
  (newline)
  (display (string-append "―――――――――――― " label " ―――――――――――"))
  (newline)
  (for-each (lambda (expr-input-target)
             (let* ((expr-in (car expr-input-target))
                    (expr-out (op expr-in))
                    (expr-target (cadr expr-input-target))
                    (success (equal? expr-target expr-out)))
               (display "[")
               (display (if success
                         "\033[32mSUCCESS!\033[0m"
                         "\033[31mFAILURE!\033[0m"))
               (display "] ")
               (display expr-in)
               (display " => ")
               (display expr-out)
               (if (not success)
                 (begin
                   (display " [expected: ")
                   (display expr-target)
                   (display "]")))
               (newline)))
    expr-input-target-list)
  (newline))

; Run tests (test cases were vibe coded, cos who wants to manually write 70 test cases?)
(run-tests "Simplification & Algebra" simplify
  (list
    ;;; Identity Properties
    (list '(x + 0) 'x)
    (list '(0 + x) 'x)
    (list '(x * 1) 'x)
    (list '(1 * x) 'x)
    (list '(x ** 1) 'x)
    ;;; Absorber Properties
    (list '(x * 0) 0)
    (list '(0 * x) 0)
    (list '(0 / x) 0)
    (list '(0 ** x) 0)
    (list '(x ** 0) 1)
    ;;; Associative Folding (The complex simplify logic)
    ;;; (3 + (4 + x)) -> (7 + x)
    (list '(3 + (4 + x)) '(7 + x))
    ;;; (x + (3 + 4)) -> (x + 7) (Handled by standard recursion)
    (list '(x + (3 + 4)) '(x + 7))
    ;;; (3 * (4 * x)) -> (12 * x)
    (list '(3 * (4 * x)) '(12 * x))
    ;;; Inverse/Subtraction Logic
    (list '(x - 0) 'x)
    (list '(x - x) 0) ; Tests inverse property in create-make-expr
    (list '(x / x) 1) ; Tests inverse property for div
    (list '(x - (0 - y)) '(x + y)) ; Double negative catch
    ;;; Logarithm Properties
    (list '(x // x) 1) ; log_x(x) = 1
    (list '(x // 1) 0) ; log_x(1) = 0
    (list '(e // (e ** x)) 'x))) ; ln(e^x) = x

(run-tests "Differentiation Rules (d/dx)" (lambda (expr) (deriv expr 'x))
  (list
    ;;; Constants and Variables
    (list '5 0)
    (list 'x 1)
    (list 'y 0)
    ;;; Sum and Difference Rule
    (list '(x + x) 2) ; 1 + 1 folds to 2
    (list '(x - x) 0) ; 1 - 1 folds to 0
    (list '((3 * x) + (2 * x)) 5) ; 3 + 2 folds to 5
    ;;; Product Rule: d(uv) = u'v + uv'
    ;;; d(x*x) = 1*x + x*1 = x + x
    (list '(x * x) '(x + x))
    ;;; Power Rule: d(u^n) = n * u^(n-1) * u'
    (list '(x ** 5) '(5 * (x ** 4)))
    (list '(x ** -1) '(-1 * (x ** -2)))
    ;;; Quotient Rule: d(u/v) = (u'v - uv') / v^2
    ;;; d(1/x) = (0*x - 1*1)/x^2 = -1/x^2
    (list '(1 / x) '(-1 / (x ** 2)))
    ;;; Exponential Rule (Base e): d(e^u) = e^u * u'
    (list '(e ** (x ** 2)) '((e ** (x ** 2)) * (2 * x)))
    ;;; Exponential Rule (Constant Base): d(a^u) = a^u * ln(a) * u'
    ;;; d(2^x) = 2^x * ln(2) * 1
    (list '(2 ** x) '((2 ** x) * (e // 2)))
    ;;; General Exponential: d(u^v)
    ;;; d(x^x) = x^x * (ln(x) + 1)
    ;;; Note: Engine order is (v' * ln u) + (v * (u'/u))
    ;;; = (1 * ln x) + (x * (1/x)) = ln x + 1
    (list '(x ** x) '((x ** x) * ((e // x) + 1)))
    ;;; Logarithms: d(ln u) = u'/u
    (list '(e // x) '(1 / x))
    ;;; Chain Rule Log: d(ln(x^2)) = 2x / x^2 = 2/x
    ;;; Implementation: deriv(x^2) / (x^2 * ln(e)) -> 2x / x^2 -> 2 * (1/x) -> 2/x
    ;;; Note: The engine simplifier for div isn't a full CAS, so it might not reduce 2x/x^2 fully.
    ;;; Let's trace expected: make-div(2x, x^2)
    (list '(e // (x ** 2)) '(2 / x)) ; Wait, ln(x^2) simplifies to 2*ln(x) BEFORE deriv called?
    ; If so, d(2*ln(x)) = 2 * (1/x).
    ;;; General Logarithm: d(log_b v)
    ;;; d(log_10 x) = 1 / (x * ln(10))
    (list '(10 // x) '(1 / (x * (e // 10))))))
