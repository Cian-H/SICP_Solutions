(define (sort l p) (hybridsort l p))
(define (sort-ascending l) (hybridsort l <=)) ; Note: must be <= for stable sort
(define (sort-descending l) (hybridsort l >))
