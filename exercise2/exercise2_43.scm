(load "lib/functools.scm")

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
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                  (adjoin-position new-row k rest-of-queens))
              (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;;; God damn! This one takes a few seconds even on a modern CPU!
;;; That is an IMPRESSIVE level of inefficiency! Let's do some diagnosing.
;
;;; ORIGINAL
; (flatmap
;   (lambda (new-row)
;     (map (lambda (rest-of-queens)
;           (adjoin-position new-row k rest-of-queens))
;       (queen-cols (- k 1)))) <= This is being called as soon as we find a valid position
;   (enumerate-interval 1 board-size))
;
;;; NEW
; (flatmap
;   (lambda (rest-of-queens)
;     (map
;       (lambda (new-row)
;         (adjoin-position new-row k rest-of-queens))
;       (enumerate-interval 1 board-size)))
;   (queen-cols (- k 1))) <= This is getting re-run on the entire board every time we recurse
;
;;; The original algorithm is roughly O(n!) where n is the number of rows, because it re-searches
;;; the space but shrinks the space by 1 on every recursion. By forcing the algorithm to recurse
;;; for the whole board on every recursion, the complexity explodes from O(n!) in the original code
;;; to O(n!*(n^n)) for this exercise's version. This explains how we're going from microseconds to
;;; several seconds for execution time: for an n of 8 this version should be 16777216x slower.

(display (queens 8))
