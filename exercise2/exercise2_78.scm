;;; Here, i'm going to modify my existing `typing.scm` module and explain how i plan to use it
;;; to solve the requirements of this exercise.

;;; I have an optimised, functional bstree i can use to track my types so i might as well use it!
(load "lib/bstree.scm")

;;; First, i'm adding a predicate for primitives, and a procedure that converts primitives to a
;;; tag that matches how my "normal" tagged types work.
(define (primitive? x)
  (or
    (number? x)
    (symbol? x)
    (pair? x)
    (boolean? x)
    (string? x)
    (null? x)
    (procedure? x)
    (vector? x)))

(define (primitive->tag x)
  (cond
    ((number? x) 'scheme-number)
    ((symbol? x) 'scheme-symbol)
    ((boolean? x) 'scheme-boolean)
    ((string? x) 'scheme-string)
    ((null? x) 'scheme-null)
    ((procedure? x) 'scheme-procedure)
    ((vector? x) 'scheme-vector)
    ((pair? x) 'scheme-pair)
    (else (error "`primitive->tag` given non-primitive: " x))))

;;; I need to add a registry for types, so that i can track what symbols are types.
;;; Ideally, this would be a hash table, but we don't have that in R5RS scheme so
;;; bstree is the best we can do here.
(define *types*
  (bstree string<=?
    "scheme-number"
    "scheme-symbol"
    "scheme-boolean"
    "scheme-string"
    "scheme-null"
    "scheme-procedure"
    "scheme-vector"
    "scheme-pair"))

;;; I also want some predicates to abstract away checking it a pair is a typed item
(define (type? s)
  (bstree-contains? *types* (symbol->string s)))

(define (typed? x)
  (and (pair? x) (type? (car x))))

;;; If a type doesn't exist, lets create a way to register it in our registry
(define (register-type s)
  (if (not (type? s))
    (set! *types* (bstree-insert *types* (symbol->string s)))))

;;; Now, when wrapping the type (in my version of `attach-tag`) i want to basically just create a
;;; guard that enforces the type of the primitive and registers the symbol as a type tag
;;; NOTE: `set!` is outside the scope of ch2 but allows us to do some cool stuff here, so i'm
;;; sticking with it.
(define (type-wrap s x)
  (if (and (primitive? x) (eq? s (primitive->tag x)))
    x
    (begin
      (register-type s)
      (cons s x))))

;;; NOTE: Some might argue that i'm deviating from the intent of the exercise by making the type
;;; system work how it does above. The above procedure allows us to actually wrap primitives if we
;;; try to tag them as a type that *doesn't* match the tags i've defined (e.g: trying to tag a
;;; 'number as a 'decimal or something like that). However, i consider this a FEATURE, not a bug!
;;; There are many scenarios where we might WANT to wrap a primitive as a type, e.g: if we were
;;; writing a module for doing fixed-point arithmetic. So, i will defend this architectural
;;; decision to the hilt.

;;; Next, i'm going to tweak `type-of` so that it returns the correct symbol for primitives, being
;;; sure to handle the edge-case of pairs that DO NOT start with type tags.
(define (type-of x)
  (let ((not-pair (not (pair? x))))
    (cond
      ((and not-pair (primitive? x)) (primitive->tag x))
      (not-pair (error "procedure `type-of` only accepts non-pair primitives and pairs!"))
      ((typed? x) (car x))
      (else 'pair))))

;;; Finally, i want the type unwrapping procedure to pass through any primitives that arent a
;;; type tagged item
(define (type-unwrap x)
  (if (typed? x)
    (cdr x)
    x))
