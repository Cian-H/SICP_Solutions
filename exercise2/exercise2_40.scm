(load "lib/math.scm")
(load "lib/functools.scm")

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs-original n)
  (map make-pair-sum
    (filter prime-sum?
      (flatmap
        (lambda (i)
          (map (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))))

(define (test-procedure proc label n)
  (display "-------------------------------------------")
  (newline)
  (display "Testing procedure: ")
  (display label)
  (newline)
  (display "-------------------------------------------")
  (newline)

  (define (test n)
    (display "Testing prime-sum-pairs procedure for n = ")
    (display n)
    (newline)
    (display "Result: ")
    (display (proc n))
    (newline)
    (display "-------------------------------------------")
    (newline))

  (define (iter i)
    (if (<= i n)
      (begin (test i) (iter (+ i 1)))
      #t))

  (iter 2))

(test-procedure prime-sum-pairs-original "Original" 6)
(newline)

(define (unique-pairs n)
  (flatmap (lambda (i)
            (map (lambda (j) (list i j))
              (enumerate-interval 1 i)))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (filter
    (lambda (x) (prime? (caddr x)))
    (map (lambda (x) (append x (list (apply + x)))) (unique-pairs n))))

(test-procedure prime-sum-pairs "New" 6)
(newline)
