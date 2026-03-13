;;; Data Directed Generics
(define (complex-install-polar-package)
  (define (from-mag-ang r a) (cons r a))
  (define (from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (wrap x) (type-wrap 'polar x))

  (put 'from-mag-ang '(polar) (lambda (x y) (wrap (from-mag-ang x y))))
  (put 'from-real-imag '(polar) (lambda (x y) (wrap (from-real-imag x y))))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  'ok)

;;; Message Passing Generics
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) (* r (cos a)))
      ((eq? op 'imag-part) (* r (sin a)))
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;; Looking at these implementations: I think that if you're regularly adding new operations the
;;; data directed style is better, whereas the message passing style is better if you're regularly
;;; adding new types. This is because the data directed technique has the significant advantage of
;;; making it easy for us to build methods that use other, previously defined methods. If we plan
;;; to build systems filled with large, complex, rich methods then this is an extremely important
;;; capability to preserve. In contrast: if we intend to regularly add new types with the same
;;; small number of simple methods then the message passing approach makes this quick and painless;
;;; we can just bash out 10 lines of code and be done with it.
;;;
;;; On top of this: if we consider the WAYS these approaches might be most cleanly built out into
;;; more complex systems. In the data directed generics the main points of friction arise when
;;; CREATING new types, we are more likely to prefer adding more complex operations to facilitate
;;; interactions between types. On the other hand: the message passing style creates some
;;; relatively hard encapsulation boundaries around the operations, making them difficult to reuse.
;;; We could fix this by creating internal procedure definitions, but this still forces a layer of
;;; encapsulation that makes interactiosn with other types difficult. In this scenario, we are
;;; more likely to lean towards making many smaller types, with mediator types to facilitate
;;; compatibility for new interactions.
;;;
;;; Effectively, in modern programming terms: the data directed style leans towards becoming "java
;;; style" OOP, whereas the message passing style leans towards becoming "smalltalk style" OOP.
