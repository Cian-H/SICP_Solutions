(load "lib/functools.scm")

(define (element-of-set? x set)
  (and (list-index (lambda (i) (equal? i x)) set) #t))

(define (adjoin-set x set)
  (if (element-of-set? x set) set (cons x set)))

(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
    (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (unique (append set1 set2)))

(define test-sets
  (list
    (list (list '(1 2 3) '(3 4 5)) (list '(1 2 3 4 5) '(3)))
    (list (list '(a b c) '(d e f)) (list '(a b c d e f) '()))
    (list (list '(1 2) '(1 2)) (list '(1 2) '(1 2)))))

(define color-green "\x1b[32m")
(define color-red   "\x1b[31m")
(define color-reset "\x1b[0m")

(define (set-equal? s1 s2)
  (let ((is-subset? (lambda (a b)
                     (fold-left (lambda (acc x) (and acc (element-of-set? x b))) #t a))))
    (and (= (length s1) (length s2))
         (is-subset? s1 s2)
         (is-subset? s2 s1))))

(define (run-tests)
  (for-each
   (lambda (test-case)
     (let* ((inputs (car test-case))
            (expected (cadr test-case))
            (s1 (car inputs))
            (s2 (cadr inputs))
            (exp-union (car expected))
            (exp-inter (cadr expected))
            (res-union (union-set s1 s2))
            (res-inter (intersection-set s1 s2)))
       (display "Testing sets: ") (display s1) (display " and ") (display s2) (newline)
       (display "  Union: ")
       (if (set-equal? res-union exp-union)
           (begin (display color-green) (display "PASS") (display color-reset))
           (begin (display color-red) (display "FAIL") (display color-reset)
                  (display " (Expected ") (display exp-union) (display " got ") (display res-union) (display ")")))
       (newline)
       (display "  Intersection: ")
       (if (set-equal? res-inter exp-inter)
           (begin (display color-green) (display "PASS") (display color-reset))
           (begin (display color-red) (display "FAIL") (display color-reset)
                  (display " (Expected ") (display exp-inter) (display " got ") (display res-inter) (display ")")))
       (newline)
       (newline)))
   test-sets))

(run-tests)
