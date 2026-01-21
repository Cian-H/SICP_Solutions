(define (fringe t) ; grumble, grumble, damn 70s programmers. Call it `flatten` dammit!
  (cond ((null? t) '())
    ((not (pair? t)) (list t))
    (else (append (fringe (car t)) (fringe (cdr t))))))

(define x (list (list 1 2) (list 3 4)))

(newline)
(display (fringe x))
(newline)
(display (fringe (list x x)))
(newline)
