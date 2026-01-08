(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;;; (car (cons x y))
;=> (car (lambda (m) (m x y)))
;=> ((lambda (m) (m x y)) (lambda (p q) p))
;=> ((lambda (p q) p) x y)
;=> x

(define (cdr z)
  (z (lambda (p q) q)))

(define (bool->string b)
  (if b "true" "false"))

(define n 1)
(define d 8)
(define r (cons n d))

(newline)
(display "r=")
(display r)
(display (string-append "; n=" (number->string n) "; d=" (number->string d) "; (car r)=>" (number->string (car r)) "; (cdr r)=>" (number->string (cdr r))))
(newline)
(display (string-append "(= (car r) n)? " (bool->string (= (car r) n))))
(newline)
(display (string-append "(= (cdr r) d)? " (bool->string (= (cdr r) d))))
