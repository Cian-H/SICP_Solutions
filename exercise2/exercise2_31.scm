(define (tree-map f t)
  (if (pair? t)
    (map (lambda (x) (tree-map f x)) t)
    (f t)))

(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))

(display (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
