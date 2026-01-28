(define (same-type? x y)
  (let ((predicates (list boolean? symbol? number? procedure? char? string? null? pair?)))
    (let loop ((preds predicates))
      (cond
        ((null? preds) #f)
        ((and ((car preds) x) ((car preds) y)) #t)
        (((car preds) x) #f)
        (((car preds) y) #f)
        (else (loop (cdr preds)))))))

(define (xnor a b)
  (or (and a b) (and (not a) (not b))))

(define (equal? x y)
  (cond
    ((eq? x y) #t)
    ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
    ((same-type? x y) (cond
                       ((boolean? x) (xnor x y))
                       ((number? x) (= x y))
                       ((char? x) (char=? x y))
                       ((string? x) (string=? x y))
                       (else #f)))
    (else #f)))
