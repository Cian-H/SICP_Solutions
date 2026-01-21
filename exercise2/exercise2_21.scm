(define l (list 1 2 3 4))

(define (square-list items)
  (if (null? items)
    nil
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(display (square-list l))
(newline)

(define (square-list items)
  (map (lambda (i) (* i i)) items))

(display (square-list l))
(newline)
