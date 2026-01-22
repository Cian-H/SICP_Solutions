(load "lib/functools.scm")

(define t (list 1 2
           (list 3 4 5
             (list 6 7)
             (list 8 9)
             10
             (list 11 (list 12 13))
             14
             (list 15 16)
             (list 17 18
               (list 19 20 21)
               (list 22 (list 23 24) (list 25) 26)))
           27
           28
           (list 29 30)
           31
           (list 32)))

;;; The original `count-leaves`, for testing
(define (count-leaves x)
  (cond ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x))
           (count-leaves (cdr x))))))

(newline)
(display (count-leaves t))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (node) (if (pair? node) (count-leaves node) 1)) t)))

(newline)
(display (count-leaves t))

;;; My own `count-leaves`, which my python-addled brain thinks makes much more sense
(define (count-leaves t)
  (length (enumerate-tree t)))

(newline)
(display (count-leaves t))
(newline)
