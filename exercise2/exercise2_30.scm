(define (square-tree t)
  (if (pair? t)
    (map (lambda (x) (square-tree x)) t)
    (* t t)))

(display (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
