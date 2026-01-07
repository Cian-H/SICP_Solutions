(define (take lst n)
  (if (or (null? lst) (= n 0))
    '()
    (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (null? lst) (= n 0))
    lst
    (drop (cdr lst) (- n 1))))

;; Merge sort implementation
(define (sort items pred)
  (define (merge left right)
    (cond ((null? left) right)
      ((null? right) left)
      ((pred (car left) (car right)) (cons (car left) (merge (cdr left) right)))
      (else (cons (car right) (merge left (cdr right))))))

  (let ((len (length items)))
    (if (<= len 1)
      items
      (let ((mid (quotient len 2)))
        (merge (sort (take items mid) pred)
          (sort (drop items mid) pred))))))
