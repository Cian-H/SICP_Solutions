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

(define (list-index pred l)
  (define (iter lt i)
    (cond
      ((null? lt) #f)
      ((pred (car lt)) i)
      (else (iter (cdr lt) (+ i 1)))))
  (iter l 0))

(define (take l n)
  (if (or (null? l) (= n 0)) '() (cons (car l) (take (cdr l) (- n 1)))))

(define (take-while pred l)
  (if (or (null? l) (not (pred (car l))))
    '()
    (cons (car l) (take-while pred (cdr l)))))

(define (append-reverse rev tail)
  (if (null? rev)
    tail
    (append-reverse (cdr rev) (cons (car rev) tail))))

(define (remove i s)
  (filter (lambda (x) (not (= x i))) s))

(define (drop l n)
  (if (or (null? l) (= n 0)) l (drop (cdr l) (- n 1))))

(define (last l)
  (if (null? (cdr l)) (car l) (last (cdr l))))

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
    (cond
      ((null? (cdr l1)) (cons (car l1) l2))
      ((< i n) (iter (+ i 1) (cdr l1) (cons (car l1) l2)))
      (else (cons (reverse l2) l1))))
  (if (not (pair? l)) (error "Split only accepts pairs!"))
  (iter 0 l '()))

;; A specialised algoritm called the "tortoise and hare" algorithm for spltting lists in half
(define (split-halves lst)
  (define (iter slow fast acc)
    (if (or (null? fast) (null? (cdr fast)))
      (cons (reverse acc) slow)
      (iter (cdr slow)
        (cddr fast)
        (cons (car slow) acc))))
  (iter lst lst '()))

(define (wrap-slide l n)
  (let ((split-list (split l n)))
    (append (cdr split-list) (car split-list))))
