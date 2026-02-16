(load "lib/functools.scm")
(load "lib/sort.scm")

(define (sort-by-symbols l)
  (sort l (lambda (a b) (string<=? (symbol->string (car a)) (symbol->string (car b))))))

(define (attrlist->attrvec l)
  (let ((sorted (sort-by-symbols l)))
    (cons
      (list->vector (map car sorted))
      (list->vector (map cdr sorted)))))

(define (symbol-binary-search l s)
  (define target-string (symbol->string s))

  (define (iter left right)
    (if (> left right)
      #f
      (let* ((mid (+ left (quotient (- right left) 2)))
             (mid-sym (vector-ref l mid))
             (mid-string (symbol->string mid-sym)))
        (cond
          ((string=? target-string mid-string) mid)
          ((string<? target-string mid-string) (iter left (- mid 1)))
          (else (iter (+ mid 1) right))))))

  (iter 0 (- (vector-length l) 1)))

(define (get-attribute obj attr)
  (let* ((attrnames (car obj))
         (attrvals (cdr obj))
         (idx (symbol-binary-search attrnames attr)))
    (if idx (vector-ref attrvals idx) #f)))

(define (add-attribute obj attrname val)
  (let* ((name (symbol->string attrname))
         (attrnames (car obj))
         (attrvals (cdr obj))
         (len (vector-length attrnames))
         (insert-idx
           (let loop ((i 0))
             (cond
               ((= i len) i)
               ((string<? name (symbol->string (vector-ref attrnames i))) i)
               (else (loop (+ i 1))))))
         (new-attrnames (make-vector (+ len 1)))
         (new-attrvals (make-vector (+ len 1))))

    (do ((i 0 (+ i 1)))
      ((= i insert-idx))
      (vector-set! new-attrnames i (vector-ref attrnames i))
      (vector-set! new-attrvals i (vector-ref attrvals i)))
    (vector-set! new-attrnames insert-idx attrname)
    (vector-set! new-attrvals insert-idx val)
    (do ((i insert-idx (+ i 1)))
      ((= i len))
      (vector-set! new-attrnames (+ i 1) (vector-ref attrnames i))
      (vector-set! new-attrvals (+ i 1) (vector-ref attrvals i)))

    (cons new-attrnames new-attrvals)))

(define (set-attribute obj attr val)
  (let* ((attrnames (car obj))
         (attrvals (cdr obj))
         (i (symbol-binary-search attrnames attr)))
    (if i
      (begin
        (vector-set! attrvals i val)
        obj)
      (let ((new-obj-data (add-attribute obj attr val)))
        (set-car! obj (car new-obj-data))
        (set-cdr! obj (cdr new-obj-data))
        obj))))

(define (make-object . attrlist)
  (let* ((self-referential-attrlist (cons (cons 'self #f) attrlist))
         (self (attrlist->attrvec self-referential-attrlist)))
    (set-attribute self 'self self)
    self))

(define-syntax object
  (syntax-rules ()
    ((_ (key value) ...)
      (make-object (cons 'key value) ...))))

(define (invoke obj method-name . args)
  (let ((method (get-attribute obj method-name)))
    (apply method (cons obj args))))
