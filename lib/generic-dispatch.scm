(load "lib/safe-load.scm")
(load "lib/dispatch-table.scm")

(define (put-coercion t1 t2 proc)
  (put 'coerce t1 t2 proc))

(define (get-coercion t1 t2)
  (get 'coerce t1 t2))

(define (apply-generic op . args)
  (let* ((types (map type-of args))
         (proc (get op types)))
    (if proc
      (apply proc (map type-unwrap args))
      (if (= (length args) 2)
        (let* ((type1 (car types))
               (type2 (cadr types))
               (a1 (car args))
               (a2 (cadr args)))
          (if (eq? type1 type2)
            (error "apply-generic: no method for types" (list op types))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond
                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                (else (error "apply-generic: no method for types" (list op types)))))))
        (error "apply-generic: no method for types" (list op types))))))
