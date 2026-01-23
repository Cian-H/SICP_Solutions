(define (dot-product v w)
  (if (not (= (length v) (length w)))
    (error "dot-product: Vector length mismatch")
    (accumulate + 0 (map * v w))))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (row) (matrix-*-vector n-cols row)) m)))
