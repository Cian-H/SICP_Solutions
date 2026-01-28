(define (cons-list->vect-list l)
  (map
    (lambda (x)
      (if (and (pair? x) (number? (car x)) (number? (cdr x)))
        (make-vect (car x) (cdr x))
        (error "List elements must be of type `cons` containing type `number`")))
    l))

(define (vect-list->segment-list l)
  (if (or (null? l) (null? (cdr l)))
    '()
    (let ((lh (car l))
          (lt (cdr l))
          (lth (cadr l)))
      (cons (make-segment lh lth) (vect-list->segment-list lt)))))

(define (cons-list->segment-list l)
  (vect-list->segment-list (cons-list->vect-list l)))

(define wave
  (append
    (cons-list->segment-list
      (list
        (cons 0.45 1)
        (cons 0.35 0.825)
        (cons 0.425 0.7)
        (cons 0.35 0.715)
        (cons 0.2 0.6)
        (cons 0 0.775)))
    (cons-list->segment-list
      (list
        (cons 0 0.65)
        (cons 0.185 0.45)
        (cons 0.385 0.585)
        (cons 0.375 0.35)
        (cons 0.25 0)))
    (cons-list->segment-list
      (list
        (cons 0.35 0)
        (cons 0.5 0.325)
        (cons 0.65 0)))
    (cons-list->segment-list
      (list
        (cons 0.75 0)
        (cons 0.625 0.35)
        (cons 0.615 0.585)
        (cons 0.815 0.45)
        (cons 1 0.25)))
    (cons-list->segment-list
      (list
        (cons 1 0.4)
        (cons 0.825 0.6)
        (cons 0.65 0.715)
        (cons 0.575 0.7)
        (cons 0.65 0.825)
        (cons 0.55 1)))
    (cons-list->segment-list
      (list
        (cons 0.45 0.8)
        (cons 0.5 0.775)
        (cons 0.55 0.8)))
    (cons-list->segment-list
      (list
        (cons 0.45 0.9)
        (cons 0.475 0.9)))
    (cons-list->segment-list
      (list
        (cons 0.55 0.9)
        (cons 0.525 0.9)))))

(define wave-painter (segments->painter wave))

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1)))
          (corner (corner-split painter (- n 1))))
      (beside (below painter up) (below right corner)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  ((square-of-four identity flip-horiz flip-vert rotate180) (corner-split painter n)))
