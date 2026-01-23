(define (enumerate-tree t)
  (cond ((null? t) nil)
    ((not (pair? t)) (list t))
    (else (append (enumerate-tree (car t))
           (enumerate-tree (cdr t))))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))
