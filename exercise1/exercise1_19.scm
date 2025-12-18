; After a bit of reading i figured out what the hell was going on here!
; They're just explaining it as if it were magic variables but, basically:
; they're creating a transformation matrix [[p+q, q], [q, p]] that defines
; a single step of the fibonacci sequence. Then, they're squaring that matrix
; so they can step through the sequence esponentially. Super clever stuff, but
; the wording and magic variables in the text make it incomprehensible to me for
; some reason. Squaring our matrix to create the new matrix
; [[p'+q', q'], [q', p']] we find that:
;     p' = p^2 + q^2
;     q' = 2pq + q^2

(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
      ((even? count)
        (fib-iter a
          b
          (+ (* p p) (* q q)) ; p'
          (+ (* 2 p q) (* q q)) ; q'
          (/ count 2)))
      (else (fib-iter (+ (* b q) (* a q) (* a p))
             (+ (* b p) (* a q))
             p
             q
             (- count 1)))))
  (fib-iter 1 0 0 1 n))
