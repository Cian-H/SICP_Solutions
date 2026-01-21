(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 1/2))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (null? kinds-of-coins)) 0)
    (else (+ (cc amount (cdr kinds-of-coins))
           (cc (- amount (car kinds-of-coins)) kinds-of-coins)))))

(define (count-change-us amount) (cc amount us-coins))
(define (count-change-uk amount) (cc amount uk-coins))

(display (count-change-us 100)) ; => 292
(newline)
(display (count-change-uk 100)) ; => 104561
(newline)
