(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; ---------------------------------------------------------
;; (a) Explain what was done above.
;; Why can't we incorporate number? and variable? into the
;; data-directed dispatch?
;;
;; Because numbers and symbols are primitives, and `car` of
;; a number or symbol will just raise an error.
;; ---------------------------------------------------------

;; ---------------------------------------------------------
;; (b) Write the procedures for derivatives of sums and
;; products, and the auxiliary code required to install
;; them in the table used by the system above.
;; ---------------------------------------------------------

(define (install-sum-deriv-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (define (make a1 a2)
    (cond
      ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2)) (+ a1 a2))
      (else (list '+ a1 a2))))

  (define (op-deriv expr var)
    (make (deriv (addend expr) var) (deriv (augend expr) var)))

  (put 'make '+ make)
  (put 'deriv '+ op-deriv)
  'done)

(define (install-product-deriv-package)
  (define (multiplier s) (car s))
  (define (multiplicand s) (cadr s))

  (define (make m1 m2)
    (cond
      ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2))
      (else (list '* m1 m2))))

  (define (op-deriv expr var)
    ((get 'make '+)
      (make (multiplier expr) (deriv (multiplicand expr) var))
      (make (deriv (multiplier expr) var) (multiplicand expr))))

  (put 'make '* make)
  (put 'deriv '* op-deriv)
  'done)

;; ---------------------------------------------------------
;; (c) Choose any additional differentiation rule that you
;; like, such as the one for exponents (Exercise 2.56),
;; and install it in this data-directed system.
;; ---------------------------------------------------------

(define (install-exponent-deriv-package)
  (define (base s) (car s))
  (define (exponent s) (cadr s))

  (define (make b e)
    (cond
      ((=number? e 0) 1)
      ((=number? b 0) 0)
      ((=number? e 1) b)
      ((and (number? b) (number? e)) (expt b e))
      (else (list '** b e))))

  (define (op-deriv expr var)
    ((get 'make '*)
      ((get 'make '*)
        (exponent expr)
        (make
          (base expr)
          ((get 'make '+) (exponent expr) -1)))
      (deriv (base expr) var)))

  (put 'make '** make)
  (put 'deriv '** op-deriv)
  'done)

;; ---------------------------------------------------------
;; (d) In this simple algebraic manipulator, the type of an
;; expression is the algebraic operator that binds it
;; together. Suppose, however, we indexed the procedures
;; in the opposite way, so that the dispatch line in deriv
;; looked like:
;;
;; ((get (operator exp) 'deriv) (operands exp) var)
;;
;; What changes, if any, do we need to make to the
;; derivation system?
;;
;; In this case we would just have to switch the order of
;; the tags in the `put` call, e.g:
;; `(put 'make '* make)` becomes
;; `(put '* 'make make)`
;; ---------------------------------------------------------
