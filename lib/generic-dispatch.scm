(load "lib/safe-load.scm")
(load "lib/dispatch-table.scm")

(define (apply-generic op . args)
  (let* ((types (map type-of args))
         (proc (get op types)))
    (if proc
      (apply proc (map type-unwrap args))
      (error "apply-generic: no method for types" (list op types)))))
