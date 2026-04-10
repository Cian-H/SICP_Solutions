(define (make-smart-constructor type-tag basic-key)
  (lambda args
    (let ((deep-simplifier (get 'simplify type-tag)))
      (if deep-simplifier
        (apply deep-simplifier args)
        (let ((basic-maker (get basic-key type-tag)))
          (if basic-maker
            (apply basic-maker args)
            (type-wrap type-tag args)))))))

(define add (make-smart-constructor '+ 'add))
(define sub (make-smart-constructor '- 'sub))
(define mul (make-smart-constructor '* 'mul))
(define div (make-smart-constructor '/ 'div))
(define exp (make-smart-constructor '** 'exp))
(define log (make-smart-constructor '// 'log))
(define ln (make-smart-constructor 'ln 'make))

(define (evaluate-constants expr)
  (cond
    ((not (pair? expr)) expr)
    ((eq? (type-of expr) 'constant)
      (cadr (type-unwrap expr)))
    (else
      (let ((type (type-of expr))
            (operands (type-unwrap expr)))
        (simplify (type-wrap type (map evaluate-constants operands)))))))

(define (substitute expr var val)
  (cond
    ((eq? expr var) val)
    ((not (pair? expr)) expr)
    ((eq? (type-of expr) 'constant) expr)
    (else
      (let ((type (type-of expr))
            (operands (type-unwrap expr)))
        (simplify
          (type-wrap type
            (map (lambda (child)
                  (substitute child var val))
              operands)))))))

(define-syntax math
  (syntax-rules ()
    ((_ expr ...)
      (parse '(expr ...)))))
