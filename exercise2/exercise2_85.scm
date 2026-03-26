(define *type-tower* '(complex scheme-real rational scheme-integer))
(define (type-tower-level t)
  (let ((types-below (memq t *type-tower*)))
    (if types-below
      (length types-below)
      #f)))

(put 'raise 'scheme-integer (lambda (i) (make-rational i 1)))
(put 'raise 'rational (lambda (r) (attach-tag 'scheme-real (/ (numer r) (denom r)))))
(put 'raise 'scheme-real (lambda (x) (make-complex-from-real-imag x 0)))
(put 'raise 'complex (lambda (x) #f))

(define (raise x)
  (let ((proc (get 'raise (list (type-tag x)))))
    (if proc
      (proc (contents x))
      #f)))

(define (scheme-real->rational x)
  (let* ((abs-x (abs x))
         (machine-epsilon 2.220446049250313e-16)
         (dynamic-tolerance (max (* abs-x machine-epsilon) 1e-320)))

    (define (iter val h-prev h-curr k-prev k-curr)
      (let* ((a (inexact->exact (floor val)))
             (rem (- val a))
             (h-next (+ (* a h-curr) h-prev))
             (k-next (+ (* a k-curr) k-prev))
             (current-approx (/ (exact->inexact h-next) k-next)))
        (if (or (<= (abs (- current-approx abs-x)) dynamic-tolerance)
             (<= rem dynamic-tolerance))
          (let ((final-h (if (< x 0) (- h-next) h-next)))
            (rational final-h k-next))
          (iter (/ 1.0 rem) h-curr h-next k-curr k-next))))

    (iter abs-x 0 1 1 0)))

(put 'lower 'complex (lambda (z) (complex-real-part z)))
(put 'lower 'scheme-real (lambda (x) (scheme-real->rational x)))
(put 'lower 'rational (lambda (r) (quotient (rational-numer r) (rational-denom r))))
(put 'lower 'scheme-integer (lambda (r) #f))

(define (project x)
  (let ((proc (get 'lower (list (type-tag x)))))
    (if proc
      (proc (contents x))
      #f)))

(define (lower x)
  (let ((projected (project x)))
    (if (and projected (equ? (raise projected) x))
      projected
      #f)))

(define (drop x)
  (if (typed? x)
    (let ((lowered (lower x)))
      (if lowered (drop lowered) x))
    x))

(define (highest-type type-list)
  (define (iter l max-type max-level)
    (if (null? l)
      max-type
      (let* ((cur-type (car l))
             (cur-type-level (type-tower-level cur-type))
             (tail (cdr l)))
        (if (> cur-type-level max-level)
          (iter tail cur-type cur-type-level)
          (iter tail max-type max-level)))))
  (iter type-list #f 0))

(define (raise-until x t)
  (let* ((xt (type-tag x))
         (xtl (type-tower-level xt))
         (tl (type-tower-level t)))
    (cond
      ((or (not xtl) (not tl)) #f)
      ((= xtl tl) x)
      ((< xtl tl) (raise-until (raise x) t))
      (else #f))))

(define (lowest-type type-list)
  (define (iter l min-type min-level)
    (if (null? l)
      min-type
      (let* ((cur-type (car l))
             (cur-type-level (type-tower-level cur-type))
             (tail (cdr l)))
        (if (< cur-type-level min-level)
          (iter tail cur-type cur-type-level)
          (iter tail min-type min-level)))))
  (iter type-list #f (length *type-tower*)))

(define (lower-until x t)
  (let* ((xt (type-tag x))
         (xtl (type-tower-level xt))
         (tl (type-tower-level t)))
    (cond
      ((or (not xtl) (not tl)) #f)
      ((= xtl tl) x)
      ((> xtl tl) (lower-until (lower x) t))
      (else #f))))

(define (coerce-list type-tags args)
  (define (iter bump-proc target-type tl vl acc)
    (if (or (null? tl) (null? vl))
      acc
      (let ((tlh (car tl)))
        (if (eq? target-type tlh)
          (iter target-type (cdr tl) (cdr vl) (cons (car vl) acc))
          (let ((equalized (bump-proc (car vl) target-type)))
            (if equalized
              (iter bump-proc target-type (cdr tl) (cdr vl) (cons equalized acc))
              #f))))))

  (reverse
    (or
      (iter raise-until (highest-type type-tags) type-tags args '())
      ;; Since lower only works if no precision is lost, we can now coerce
      ;; to lower too if no raising path is found
      (iter lower-until (lowest-type type-tags) type-tags args '()))))

(define (all-same-type? tags)
  (if (null? tags)
    #t
    (let ((first-tag (car tags)))
      (define (check rest-tags)
        (cond ((null? rest-tags) #t)
          ((not (eq? first-tag (car rest-tags))) #f)
          (else (check (cdr rest-tags)))))
      (check (cdr tags)))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
      (drop (apply proc (map contents args)))
      (if (all-same-type? type-tags)
        (error "No method for these types" (list op type-tags))
        (let ((coerced (coerce-list type-tags args)))
          (if coerced
            (drop (apply apply-generic (cons op coerced)))
            (error "No method for these types" (list op type-tags))))))))
