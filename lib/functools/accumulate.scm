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

(define (count pred seq)
  (fold-left (lambda (acc x) (if (pred x) (+ acc 1) acc)) 0 seq))

(define (frequencies lst sort-pred eq-pred)
  (define (iter l x acc)
    (if (null? l)
      acc
      (let ((lh (car l))
            (lt (cdr l)))
        (cond
          ((eq? x '__frequency_placeholder) (iter lt lh (cons (cons lh 1) acc)))
          ((eq-pred x lh)
            (let* ((acch (car acc))
                   (acch_value (car acch))
                   (acch_count (cdr acch))
                   (acct (cdr acc)))
              (iter lt x (cons (cons acch_value (+ acch_count 1)) acct))))
          (else (iter lt lh (cons (cons lh 1) acc)))))))
  (iter (sort lst sort-pred) '__frequency_placeholder '()))
