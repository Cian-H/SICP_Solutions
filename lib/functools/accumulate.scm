(define (accumulate op acc s)
  (if (null? s)
    acc
    (op (car s) (accumulate op acc (cdr s)))))

(define (accumulate-n op acc seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op acc (map car seqs))
      (accumulate-n op acc (map cdr seqs)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(define (every pred seq)
  (cond
    ((null? seq) #t)
    ((pred (car seq)) (every pred (cdr seq)))
    (else #f)))
