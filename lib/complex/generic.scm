(define (complex-apply-generic op . args)
  (let* ((types (map type-of args))
         (proc (get op types)))
    (if proc
      (apply proc (map type-unwrap args))
      (error "complex-apply-generic: no method for types" (list op types)))))
