(define (split transform1 transform2)
  (define (rec painter n)
    (if (= n 0)
      painter
      (let ((smaller (rec painter (- n 1))))
        (transform1 painter (transform2 smaller smaller)))))
  rec)

(define right-split (split beside below))
(define up-split (split below beside))
