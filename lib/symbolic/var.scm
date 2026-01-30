(define (variable? x)
  (and (symbol? x) (not (constant-symbol? x))))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expr num) (and (number? expr) (= expr num)))
