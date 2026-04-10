(define (contains-var? expr var)
  (cond ((eq? expr var) #t)
    ((not (pair? expr)) #f)
    (else (any (lambda (child) (contains-var? child var))
           (type-unwrap expr)))))

(define (solve equation var)
  (let ((lhs (cadr equation))
        (rhs (caddr equation)))
    (cond
      ;; Solved!
      ((eq? lhs var) equation)
      ;; Variable on the right, flip it
      ((contains-var? rhs var)
        (solve (list '= rhs lhs) var))
      ;; Recurse: Dispatch to the specific operator's isolation logic
      ((pair? lhs)
        (let* ((op (type-of lhs))
               (isolate-proc (get 'isolate op)))
          (if isolate-proc
            (isolate-proc (type-unwrap lhs) rhs var)
            (error "Don't know how to isolate operator:" op))))
      (else (error "Variable not found in equation" var)))))
