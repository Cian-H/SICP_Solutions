;;; I do understand this is over-engineered, but i was really enhoying the challenge of creating
;;; A fully functional symbolic algebra engine with working derivative calculus
(load "lib/functools.scm")

;;; Start by defining ops
(define supported-ops '(+ - * / ** //)) ; '// used as non-standard infix operator for log
(define (supported-op? op) (if (memq op supported-ops) #t #f))

(define op-precedence
  (list
    (list '+ 10)
    (list '- 10)
    (list '* 10)
    (list '/ 10)
    (list '** 10)
    (list '// 10)))

(define (lookup-maker l label)
  (lambda (s)
    (define (iter l)
      (let ((lh (car l))
            (lt (cdr l)))
        (cond
          ((null? lh) (error (string-concat "Requested key" (symbol->string s) "not found in " label)))
          ((eq? s (car lh)) (cdr lh))
          (else (iter lt)))))
    (iter l)))

(define op-precedence-lookup (lookup-maker op-precedence "op-precedence"))
(define (op->precedence s) (car (op-precedence-lookup s)))

(define (op-precedence op)
  (cond ((memq op '(+ -)) 10) ; Lowest binding
    ((memq op '(* / //)) 20) ; Medium binding
    ((memq op '(**)) 30) ; Tightest binding
    (else 999)))

(define (lowest-precedence-op expr)
  (let* ((ops (filter supported-op? expr))
         (precs (map op-precedence ops))
         (min-prec (apply min precs)))
    (let ((sample-op (list-ref ops (list-index (lambda (p) (= p min-prec)) precs))))
      (if (right-associative? sample-op)
        (list-ref ops (list-index (lambda (p) (= p min-prec)) precs))
        (list-ref ops (list-index-last (lambda (p) (= p min-prec)) precs))))))

(define (list-index-last pred lst)
  (define (iter l idx last-match)
    (cond ((null? l) last-match)
      ((pred (car l)) (iter (cdr l) (+ idx 1) idx))
      (else (iter (cdr l) (+ idx 1) last-match))))
  (iter lst 0 #f))

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

(define constant-lookup (lookup-maker constants-values "constants-values"))
(define (constant->value s) (simplify (constant-lookup s)))

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

;;; Base expression handling logic
; Basic predicates and selectors
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
(define (exponent expr) (get-suffix '** expr))

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

(define (type-score x)
  (cond ((number? x) 1)
    ((symbol? x) 2)
    ((pair? x) 3)
    (else 4)))

(define (expr<? a b)
  (let ((score-a (type-score a))
        (score-b (type-score b)))
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

;;; Expression Simplifier
; First, we define associativity rules
(define right-associative-ops '(+ * **))
(define left-associative-ops '(- / //))

(define (right-associative? s) (if (memq s right-associative-ops) #t #f))
(define (left-associative? s) (if (memq s left-associative-ops) #t #f))

(define (get-prefix s expr)
  (let ((res (cond
              ((right-associative? s) (take-while (lambda (x) (not (eq? x s))) expr))
              ((left-associative? s) (reverse (cdr (memq s (reverse expr)))))
              (else (error "Unknown associativity" s)))))
    (cond
      ((null? res) (error "Empty prefix for operator" s))
      ((null? (cdr res)) (car res))
      (else res))))

(define (get-suffix s expr)
  (let ((res (cond
              ((right-associative? s) (cdr (memq s expr)))
              ((left-associative? s) (reverse (take-while (lambda (x) (not (eq? x s))) (reverse expr))))
              (else (error "Unknown associativity" s)))))
    (cond
      ((null? res) (error "Empty suffix for operator" s))
      ((null? (cdr res)) (car res))
      (else res))))
; Then, we define simplification rules for specific subtypes of operators
(define (simplify-unique-op a b maker-std _maker-inv _op-std _op-inv _predicate _left-sel _right-sel)
  (maker-std a b))

(define (simplify-accumulator-op a b maker-std _maker-inv op-std _op-inv predicate left-sel right-sel)
  (cond
    [(and (number? a) (pair? b) (predicate b) (number? (left-sel b))) (maker-std (op-std a (left-sel b)) (right-sel b))]
    [(and (number? b) (pair? a) (predicate a) (number? (right-sel a))) (maker-std (left-sel a) (op-std b (right-sel a)))]
    [else (maker-std a b)]))

(define (simplify-debtor-op a b maker-std maker-inv op-std op-inv predicate left-sel right-sel)
  (cond
    [(and (number? a) (pair? b) (predicate b) (number? (left-sel b))) (maker-inv (op-std a (left-sel b)) (right-sel b))]
    [(and (number? b) (pair? a) (predicate a) (number? (right-sel a))) (maker-std (left-sel a) (op-inv b (right-sel a)))]
    [else (maker-std a b)]))

(define simplification-rules
  (list
    (list '+ simplify-accumulator-op make-sum inverse-sum + - sum? addend augend)
    (list '- simplify-debtor-op make-sub inverse-sub - + sub? minuend subtrahend)
    (list '* simplify-accumulator-op make-product inverse-product * / product? multiplier multiplicand)
    (list '/ simplify-debtor-op make-div inverse-div / * div? dividend divisor)
    (list '** simplify-unique-op make-exponentiation #f #f #f exponentiation? base exponent)
    (list '// simplify-unique-op make-log #f #f #f log? log-base log-val)))

(define simplification-rule-lookup (lookup-maker simplification-rules "simplification-rules"))

(define (op->simplification-rule s)
  (let* ((rule (simplification-rule-lookup s))
         (rule-op (car rule))
         (rule-args (cdr rule)))
    (lambda (a b)
      (apply rule-op (append (list a b) rule-args)))))

(define (simplify expr)
  (cond
    ((null? expr) expr)
    ((number? expr) expr)
    ((symbol? expr) expr)
    ((null? (cdr expr)) (simplify (car expr)))
    ((pair? expr) (let ((op (lowest-precedence-op expr)))
                   (let ((lhs (simplify (get-prefix op expr)))
                         (rhs (simplify (get-suffix op expr))))
                     ((op->simplification-rule op) lhs rhs))))
    (else expr)))

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
    ((number? expr) 0)
    ((variable? expr) (if (same-variable? expr var) 1 0))
    ((constant-symbol? expr) 0)

    ((pair? expr) (let* ((op (lowest-precedence-op expr))
                         (u (get-prefix op expr))
                         (v (get-suffix op expr)))
                   (cond
                     ((eq? op '+) (make-sum (deriv u var) (deriv v var)))
                     ((eq? op '-) (make-sub (deriv u var) (deriv v var)))
                     ((eq? op '*) (make-sum (make-product u (deriv v var))
                                   (make-product (deriv u var) v)))
                     ((eq? op '/) (make-div (make-sub (make-product v (deriv u var))
                                             (make-product u (deriv v var)))
                                   (make-exponentiation v 2)))
                     ((eq? op '**) (let ((base u) (exponent v))
                                    (cond ((number? exponent) (deriv-exponential-power-rule base exponent var))
                                      ((eq? base 'e) (deriv-exponential-exponential-rule expr exponent var))
                                      (else (deriv-exponential-general base exponent var)))))
                     ((eq? op '//) (cond
                                    ((or (number? u) (constant-symbol? u)) (make-div (deriv v var) (make-product v (make-ln u))))
                                    (else (deriv (make-div (make-ln v) (make-ln u)) var))))
                     (else (error "Unknown operator:" op)))))
    (else (error "Unknown expression type" expr))))

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
    ;;; Associativity tests
    (list '(1 - 2 + 3) 2)
    (list '(0 - a - b) '(0 - (a + b)))
    (list '(a + b) '(a + b))
    (list '(a + b + c) '(a + (b + c)))
    (list '(a + b + c + d) '(a + (b + (c + d))))
    (list '(a + b + c + d + 4) '(a + (b + (c + (4 + d)))))
    (list '((a + b) + (c + d)) '((a + b) + (c + d)))
    (list '(a - b) '(a - b))
    (list '(a - b - c) '((a - b) - c))
    (list '(a - b - c - d) '(((a - b) - c) - d))
    (list '(a - b - c - d - 4) '((((a - b) - c) - d) - 4))
    (list '((a - b) - (c - d)) '((a - b) - (c - d)))
    (list '(a * b) '(a * b))
    (list '(a * b * c) '(a * (b * c)))
    (list '(a * b * c * d) '(a * (b * (c * d))))
    (list '(a * b * c * d * 4) '(a * (b * (c * (4 * d)))))
    (list '((a * b) * (c * d)) '((a * b) * (c * d)))
    (list '(a / b) '(a / b))
    (list '(a / b / c) '((a / b) / c))
    (list '(a / b / c / d) '(((a / b) / c) / d))
    (list '(a / b / c / d / 4) '((((a / b) / c) / d) / 4))
    (list '((a / b) / (c / d)) '((a / b) / (c / d)))
    (list '(a ** b) '(a ** b))
    (list '(a ** b ** c) '(a ** (b ** c)))
    (list '(a ** b ** c ** d) '(a ** (b ** (c ** d))))
    (list '(a ** b ** c ** d ** 4) '(a ** (b ** (c ** (d ** 4)))))
    (list '((a ** b) ** (c ** d)) '((a ** b) ** (c ** d)))
    (list '(a // b) '(a // b))
    (list '(a // b // c) '((a // b) // c))
    (list '(a // b // c // d) '(((a // b) // c) // d))
    (list '(a // b // c // d // 4) '((((a // b) // c) // d) // 4))
    (list '((a // b) // (c // d)) '((a // b) // (c // d)))
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
    (list '(3 + (4 + x)) '(7 + x))
    (list '(x + (3 + 4)) '(7 + x))
    (list '(3 * (4 * x)) '(12 * x))
    ;;; Inverse/Subtraction Logic
    (list '(x - 0) 'x)
    (list '(x - x) 0)
    (list '(x / x) 1)
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
    (list '(x * x) '(x + x))
    ;;; Power Rule: d(u^n) = n * u^(n-1) * u'
    (list '(x ** 5) '(5 * (x ** 4)))
    (list '(x ** -1) '(-1 * (x ** -2)))
    ;;; Quotient Rule: d(u/v) = (u'v - uv') / v^2
    (list '(1 / x) '(-1 / (x ** 2)))
    ;;; Exponential Rule (Base e): d(e^u) = e^u * u'
    (list '(e ** (x ** 2)) '((2 * x) * (e ** (x ** 2))))
    ;;; Exponential Rule (Constant Base): d(a^u) = a^u * ln(a) * u'
    (list '(2 ** x) '((e // 2) * (2 ** x)))
    ;;; General Exponential: d(u^v)
    (list '(x ** x) '((1 + (e // x)) * (x ** x)))
    ;;; Logarithms: d(ln u) = u'/u
    (list '(e // x) '(1 / x))
    ;;; Chain Rule Log: d(ln(x^2)) = 2x / x^2 = 2/x
    (list '(e // (x ** 2)) '((2 * x) / (x ** 2)))
    ;;; General Logarithm: d(log_b v)
    (list '(10 // x) '(1 / (x * (e // 10))))))
