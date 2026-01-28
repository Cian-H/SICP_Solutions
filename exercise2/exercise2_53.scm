; Prediction: (a b c)
(list 'a 'b 'c) ; (a b c)

; Prediction: ((george))
(list (list 'george)) ; ((george))

; Prediction: ((y1 y2))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))

; Prediction: (y1 y2)
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)

; Prediction: #f
(pair? (car '(a short list))) ; #f

; Prediction: #f
(memq 'red '((red shoes) (blue socks))) ; #f

; Prediction: #t
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)
; Reponse: wait, what? I thought memq was supposed to be a boolean predicate?
;   Did i misunderstand? It appears to work as a guard that passes the list through if the symbol
;   is found. Begging the question: why? I'm guessing it's because the only real use for this
;   function is validating the presence of symbols for creating tagged structures. If that's the
;   case though: i'm not a fan of the fact this breaks the type invariance of this procedure.
;   That being said, a type invariant scan wouldn't be hard if we had an `any` procedure; this then
;   just becomes as simple as `(any (lambda (x) (eq? x 'red)) '(red shoes blue socks))`.
; Just to confirm this possible functionality, let's test the use of this as a basic guard for a
;   tagged type.

(begin ; adding begin here just so i can evaluate the block in conjure
  (define (valid-coord-pair? p)
    (and (pair? p) (number? (car p)) (number? (cdr p))))

  (define (cons->point x)
    (if (valid-coord-pair? x)
      (list 'point x)
      (error "`cons-point` failed: cons contains invalid coordinate values!")))

  (define (make-point x y)
    (cons->point (cons x y)))

  (define (point-guard p)
    (memq 'point p))

  (define test-point (make-point 1 2))

  (point-guard test-point)) ; (point (1 . 2))

; Ok, that does seem to work. I like this, it's a very elegant way to create a quick, custom type
;   system. Starting to see why lisp is so often described as a language you use to write your own
;   language.
; HOWEVER, i still question the value of `memq`. Instead, I could do this more efficiently,
;   robustly, and flexibly by just tagging the head for any types, basically making a type system
;   built around simple monads of (type . value). For example:

(begin
  (define (unwrap x)
    (if (pair? x)
      (let ((x-type (car x))
            (x-value (cdr x)))
        (if (symbol? x-type)
          x-value
          (error "Cannot unwrap an untagged pair")))
      (error "Cannot unwrap a primitive")))

  (define (tagged-pair? x)
    (and (pair? x) (symbol? (car x))))

  (define (match-type s x)
    (if (tagged-pair? x)
      (let ((x-type (car x)))
        (if (eq? x-type s) x #f))
      (error "Can only match-type on a tagged pair")))

  (define (match-unwrap s x)
    (if (match-type s x)
      (unwrap x)
      (error (string-append
              "Type mismatch: Expected "
              (symbol->string s)
              " got "
              (symbol->string (car x))))))

  (define (valid-coord-pair? p)
    (and (pair? p) (number? (car p)) (number? (cdr p))))

  (define (cons->point x)
    (if (valid-coord-pair? x)
      (cons 'point x)
      (error "`cons-point` failed: cons contains invalid coordinate values!")))

  (define (make-point x y)
    (cons->point (cons x y)))

  (define (point-guard p)
    (match-type 'point p))

  (define test-point (make-point 1 2))

  (point-guard test-point)) ; (point 1 . 2)

; This works well and gives access to a bunch of clearer, efficient, monadic operators like
; `unwrap` and `match`, with the possibility of other additions to the system such as `lift`.
