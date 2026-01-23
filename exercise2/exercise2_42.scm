(load "lib/functools.scm")
(load "lib/matrices.scm")

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define (safe? k positions)
  (let ((new-queen (car positions))
        (others (cdr positions)))
    (accumulate
      (lambda (q rest-are-safe)
        (and rest-are-safe
          (not (= (car new-queen) (car q)))
          (not (= (abs (- (car new-queen) (car q)))
                (abs (- (cdr new-queen) (cdr q)))))))
      #t
      others)))

(define empty-board '())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map
              (lambda (new-row)
                (adjoin-position new-row k rest-of-queens))
              (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(display (queens 8))
