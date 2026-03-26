(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;;; a
(define (exp x y) (apply-generic 'exp x y))

; In this situation: Louis has created a BIG problem. If we attempt to run `exp` on 2 complex
; numbers then `apply-generic` looks for a `('exp '(complex complex))` method in the dispatch
; table. Because this method doesn't exist, the first `if` statement in the procedure evaluates
; the pathway that attempts type coercion as a fallback:

(if proc ; <- `proc` doesn't exist, so we go to the `else` branch where coercion type occurs
  (apply proc (map contents args))
  ...)

; THEN, it would attempt to coerce the first type and see if there is a procedure for that type
; combination:

(cond
  (t1->t2 (apply-generic op (t1->t2 a1) a2)) ; <- This branch will trigger, applying `complex->complex` and recursing
  ...)

; And now, we have just re-invoked `apply-generic` looking for a `('exp '(complex complex))`
; method but *nothing has changed!* We are now in an infinite loop, recursively coercing a
; `complex` into a `complex` and re-invoking `apply-generic` until the callstack overflows.

;;; b
; Louis's logic was incorrect, and apply generic works fine as-is. If a homogeneous method exists:
; the normal dispatch method will dispatch it without coercions. And if a homogeneous method
; *doesn't* exist then coercing a type to *itself* isn't going to fix that!

;;; c
; Just in case somebody adds an identity conversion, we can prevent this infinite loop by simply
; modifying `apply-generic` as follows:

(define (apply-generic op . args)
  (let ((proc (get op type-tags)))
    (if proc
      (apply proc (map contents args))
      (if (= (length args) 2)
        (let* ((type1 (car type-tags))
               (type2 (cadr type-tags))
               (a1 (car args))
               (a2 (cadr args))
               (t1->t2 (get-coercion type1 type2))
               (t2->t1 (get-coercion type2 type1)))
          (cond
            ((eq? type1 type2) (error "No isomorphic procedure defined for types" (list op type-tags)))
            (t1->t2 (apply-generic op (t1->t2 a1) a2))
            (t2->t1 (apply-generic op a1 (t2->t1 a2)))
            (else (error "No method for these types" (list op type-tags)))))
        (error "No method for these types" (list op type-tags))))))
