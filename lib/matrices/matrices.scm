(define (create-matrix seq stride)
  (cond ((null? seq) '())
    ((< (length seq) stride) (error "create-matrix: List length not divisible by stride"))
    (else
      (cons (take seq stride)
        (create-matrix (drop seq stride) stride)))))

(define (fill x h w)
  (create-matrix (repeat x (* h w)) w))

(define (zeros h w)
  (fill 0 h w))

(define (ones h w)
  (fill 1 h w))
