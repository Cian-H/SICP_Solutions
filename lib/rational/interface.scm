;;; Constructor
(define (rational n d) ((get 'make 'rational) n d))
;;; Accessors
(define (rational-numer x) (apply-generic 'numer x))
(define (rational-denom x) (apply-generic 'denom x))
;;; Predicates
(define (rational-equal? x y) (apply-generic 'equal? x y))
;;; Operations
(define (rational-add x y) (apply-generic 'add x y))
(define (rational-sub x y) (apply-generic 'sub x y))
(define (rational-mul x y) (apply-generic 'mul x y))
(define (rational-div x y) (apply-generic 'div x y))
