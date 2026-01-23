(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))
