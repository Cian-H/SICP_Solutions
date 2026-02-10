(load "lib/functools.scm")

; This should still work regardless of whether there are duplicates
(define (element-of-set? x set)
  (and (list-index (lambda (i) (equal? i x)) set) #t))

; If the number of duplicates doesn't matter then this should be sufficient
(define (adjoin-set x set)
  (cons x set))

; This should also be fine, as it already handles this appropriately
(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
    (else (intersection-set (cdr set1) set2))))

; Again, if the number of duplicates doesn't matter we should just append without filtering
; down to the unique values only. Half considering `(define union-set adjoin-set)` here but
; don't want to tightly couple these implementations in case i need to tweak later.
(define (union-set set1 set2)
  (append set1 set2))

(define test-sets
  (list
    (list (list '(2 3 2 1 3 2 2) '(3 3 5 4 4 4 5)) (list '(1 2 3 4 5) '(3)))
    (list (list '(a a a a a b b c) '(d e e f)) (list '(a b c d e f) '()))
    (list (list '(1 2) '(1 2 2 2 2 2)) (list '(1 2) '(1 2)))))

(define color-green "\x1b[32m")
(define color-red   "\x1b[31m")
(define color-reset "\x1b[0m")

(define (is-subset? sub super)
  (every (lambda (x) (element-of-set? x super)) sub))

(define (set-equal? s1 s2)
  (and (is-subset? s1 s2)
       (is-subset? s2 s1)))

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
       (display res-union)
       (display " (")
       (if (set-equal? (unique res-union) exp-union)
           (begin (display color-green) (display "PASS") (display color-reset))
           (begin (display color-red) (display "FAIL") (display color-reset)
                  (display " (Expected ") (display exp-union) (display " got ") (display res-union) (display ")")))
       (display ")")
       (newline)
       (display "  Intersection: ")
       (display res-inter)
       (display " (")
       (if (set-equal? (unique res-inter) exp-inter)
           (begin (display color-green) (display "PASS") (display color-reset))
           (begin (display color-red) (display "FAIL") (display color-reset)
                  (display " (Expected ") (display exp-inter) (display " got ") (display res-inter) (display ")")))
       (display ")")
       (newline)
       (newline)))
   test-sets))

(run-tests)
