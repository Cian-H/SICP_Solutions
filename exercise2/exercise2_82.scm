(define (coerce-list type-tags args)
  (define (iter-inner target-type tl vl acc)
    (if (or (null? tl) (null? vl))
      acc
      (let ((tlh (car tl)))
        (if (eq? target-type tlh)
          (iter-inner target-type (cdr tl) (cdr vl) (cons identity acc))
          (let ((t->target (get-coercion tlh target-type)))
            (if t->target
              (iter-inner target-type (cdr tl) (cdr vl) (cons t->target acc))
              #f))))))

  (define (iter-outer rem)
    (if (null? rem)
      #f
      (let ((conversions (iter-inner (car rem) type-tags args '())))
        (if conversions
          (map (lambda (f x) (f x)) (reverse conversions) args)
          (iter-outer (cdr rem))))))

  (iter-outer type-tags))

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
