(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define pos (a-plus-abs-b 10 4))
(define neg (a-plus-abs-b 10 -4))

(write (string-append (number->string pos) "==" (number->string neg) "?"))
