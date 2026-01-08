(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;; (add-1 zero)
;=> ((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (fz) (lambda (xz) xz)))
;=> (lambda (f) (lambda (x) (f (((lambda (fz) (lambda (xz) xz)) f) x))))
;=> (lambda (f) (lambda (x) (f ((lambda (xz) xz) x))))
;=> (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))
