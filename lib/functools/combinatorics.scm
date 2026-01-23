(define (repeat x n)
  (define (iter i acc)
    (if (> i 0)
      (iter (- i 1) (cons x acc))
      acc))
  (iter n '()))

(define (range start end)
  (define (iter i acc)
    (if (>= i start)
      (iter (- i 1) (cons i acc))
      acc))
  (iter (- end 1) '()))

(define (take l n)
  (if (or (null? l) (= n 0)) '() (cons (car l) (take (cdr l) (- n 1)))))

(define (remove i s)
  (filter (lambda (x) (not (= x i))) s))

(define (drop l n)
  (if (or (null? l) (= n 0)) l (drop (cdr l) (- n 1))))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
              (map (lambda (p) (cons x p))
                (permutations (remove x s))))
      s)))

(define (split l n)
  (define (iter i l1 l2)
    (if (< i n)
      (iter (+ i 1) (cdr l1) (cons (car l1) l2))
      (cons (reverse l2) l1)))
  (iter 0 l '()))

(define (wrap-slide l n)
  (let ((split-list (split l n)))
    (append (cdr split-list) (car split-list))))
