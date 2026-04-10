(define (install-symbolic/simplify-traits)
  (define (make-accumulator-simplifier type-tag inverse-tag native-op identity-val)
    (lambda (lhs rhs)
      ;; Canonical ordering for commutativity
      (if (expr<? rhs lhs)
        (let ((temp lhs))
          (set! lhs rhs)
          (set! rhs temp)))

      (let ((self-simplify (get 'simplify type-tag))
            (inv-simplify (get 'simplify inverse-tag)))
        (cond
          ((and (number? lhs) (number? rhs)) (native-op lhs rhs))
          ((and (number? lhs) (= lhs identity-val)) rhs)
          ((and (number? rhs) (= rhs identity-val)) lhs)
          ;; Product of Reciprocal / Addition of Negative: x * (1 / y) -> x / y
          ((and (pair? rhs) (eq? (type-of rhs) inverse-tag)
              (number? (car (type-unwrap rhs)))
              (= (car (type-unwrap rhs)) identity-val))
            (if inv-simplify
              (inv-simplify lhs (cadr (type-unwrap rhs)))
              (type-wrap inverse-tag (list lhs (cadr (type-unwrap rhs))))))
          ((and (number? lhs) (pair? rhs) (eq? (type-of rhs) type-tag) (number? (car (type-unwrap rhs))))
            (self-simplify (native-op lhs (car (type-unwrap rhs))) (cadr (type-unwrap rhs))))
          ((and (number? rhs) (pair? lhs) (eq? (type-of lhs) type-tag) (number? (cadr (type-unwrap lhs))))
            (self-simplify (car (type-unwrap lhs)) (native-op rhs (cadr (type-unwrap lhs)))))
          (else (type-wrap type-tag (list lhs rhs)))))))

  (define (make-debtor-simplifier type-tag inverse-tag native-op native-inv-op identity-val)
    (lambda (lhs rhs)
      (let ((self-simplify (get 'simplify type-tag))
            (inv-simplify (get 'simplify inverse-tag)))
        (cond
          ((and (number? lhs) (number? rhs)) (native-op lhs rhs))
          ((and (number? rhs) (= rhs identity-val)) lhs)
          ;; Extracting Negatives / Product of Reciprocal on LHS: (0 - x) - y -> 0 - (x + y)
          ((and (pair? lhs) (eq? (type-of lhs) type-tag)
              (number? (car (type-unwrap lhs)))
              (= (car (type-unwrap lhs)) identity-val))
            (let ((x (cadr (type-unwrap lhs))))
              (if inv-simplify
                (self-simplify identity-val (inv-simplify x rhs))
                (type-wrap type-tag (list identity-val (type-wrap inverse-tag (list x rhs)))))))
          ;; Double Debtor Property: 0 - (0 - x) -> x
          ((and (number? lhs) (= lhs identity-val)
              (pair? rhs)
              (eq? (type-of rhs) type-tag)
              (number? (car (type-unwrap rhs)))
              (= (car (type-unwrap rhs)) identity-val))
            (cadr (type-unwrap rhs)))
          ;; Inverse Extraction: x - (0 - y) -> x + y
          ((and (pair? rhs) (eq? (type-of rhs) type-tag)
              (number? (car (type-unwrap rhs)))
              (= (car (type-unwrap rhs)) identity-val))
            (if inv-simplify
              (inv-simplify lhs (cadr (type-unwrap rhs)))
              (type-wrap inverse-tag (list lhs (cadr (type-unwrap rhs))))))
          ;; Debtor Rule: a - (b - x) -> (a - b) + x
          ((and (number? lhs) (pair? rhs) (eq? (type-of rhs) type-tag) (number? (car (type-unwrap rhs))))
            (let ((new-lhs (native-op lhs (car (type-unwrap rhs))))
                  (new-rhs (cadr (type-unwrap rhs))))
              (if inv-simplify
                (inv-simplify new-lhs new-rhs)
                (type-wrap inverse-tag (list new-lhs new-rhs)))))
          (else (type-wrap type-tag (list lhs rhs)))))))

  (put 'trait 'simplify 'accumulator make-accumulator-simplifier)
  (put 'trait 'simplify 'debtor make-debtor-simplifier)
  'ok)
