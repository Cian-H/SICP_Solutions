(load "lib/bstree.scm")

(define (primitive? x)
  (or
    (number? x)
    (symbol? x)
    (boolean? x)
    (string? x)
    (null? x)
    (procedure? x)
    (vector? x)
    (pair? x)))

(define (primitive->tag x)
  (cond
    ((number? x) 'number)
    ((symbol? x) 'symbol)
    ((boolean? x) 'boolean)
    ((string? x) 'string)
    ((null? x) 'null)
    ((procedure? x) 'procedure)
    ((vector? x) 'vector)
    ((pair? x) 'pair)
    (else (error "`primitive->tag` given non-primitive: " x))))

(define *types*
  (bstree string<=?
    "number"
    "symbol"
    "boolean"
    "string"
    "null"
    "procedure"
    "vector"
    "pair"))

(define (type? s)
  (bstree-contains? *types* (symbol->string s)))

(define (typed? x)
  (and (pair? x) (type? (car x))))

(define (register-type s)
  (if (not (type? s))
    (set! *types* (bstree-insert *types* (symbol->string s)))))

(define (type-wrap s x)
  (if (and (primitive? x) (eq? s (primitive->tag x)))
    x
    (begin
      (register-type s)
      (cons s x))))

(define (type-of x)
  (let ((not-pair (not (pair? x))))
    (cond
      ((and not-pair (primitive? x)) (primitive->tag x))
      (not-pair (error "procedure `type-of` only accepts non-pair primitives and pairs!"))
      ((typed? x) (car x))
      (else 'pair))))

(define (type-unwrap x)
  (if (and (pair? x) (symbol? (car x)))
    (cdr x)
    x))
