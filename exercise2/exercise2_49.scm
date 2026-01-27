(define (cons-list->vect-list l)
  (map
    (lambda (x)
      (if (and (pair? x) (number? (car x)) (number? (cdr x)))
        (make-vect (car x) (cdr x))
        (error "List elements must be of type `cons` containing type `number`")))
    l))

(define (vect-list->segment-list l)
  (if (null? (cdr l))
    '()
    (let ((lh (car l))
          (lt (cdr l))
          (lth (cadr l)))
      (cons (make-segment lh lth) (vect-list->segment-list lt)))))

(define (cons-list->segment-list l)
  (vect-list->segment-list (cons-list->vect-list l)))

(define corners (cons-list->vect-list
                 (list
                   (cons 0 0)
                   (cons 1 0)
                   (cons 1 1)
                   (cons 0 1))))

(define midpoints (cons-list->vect-list
                   (list
                     (cons 0.5 0)
                     (cons 0 0.5)
                     (cons 0.5 1)
                     (cons 1 0.5))))

(define outline-painter
  (segments->painter (vect-list->segment-list (append corners (list (car corners))))))

(define x-painter
  (let ((bl (list-ref corners 0))
        (br (list-ref corners 1))
        (tl (list-ref corners 3))
        (tr (list-ref corners 2)))
    (segments->painter
      (list
        (make-segment bl tr)
        (make-segment tl br)))))

(define diamond-painter
  (segments->painter (vect-list->segment-list (append midpoints (list (car midpoints))))))

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
        (cons 0.55 1)))))

(define wave-painter (segments->painter wave))
