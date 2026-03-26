;;; I understand this is a subpar way to do this, but i already have a full DFS search dispatch
;;; method implemented in my *main* generic dispatcher, so this shortcut will do for this exercise;
;;; I don't really need to plan ahead for this to go in my main library.
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

(define (coerce-list type-tags args)
  (define (iter target-type tl vl acc)
    (if (or (null? tl) (null? vl))
      acc
      (let ((tlh (car tl)))
        (if (eq? target-type tlh)
          (iter target-type (cdr tl) (cdr vl) (cons (car vl) acc))
          (let ((equalized (raise-until (car vl) target-type)))
            (if equalized
              (iter target-type (cdr tl) (cdr vl) (cons equalized acc))
              #f))))))

  (reverse (iter (highest-type type-tags) type-tags args '())))

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
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (all-same-type? type-tags)
          (error "No method for these types" (list op type-tags))
          (let ((coerced (coerce-list type-tags args)))
            (if coerced
              (apply apply-generic (cons op coerced))
              (error "No method for these types" (list op type-tags)))))))))

;;; This method of coercion will only work for completely homogeneous operations, that accept
;;; arguments of all the same type! This obviously is bad if we have any heterogeneous methods
;;; that might be applicable.
