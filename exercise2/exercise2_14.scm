(load "lib/intervals.scm")

(define one (make-interval 1 1))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
    (add-interval r1 r2)))

(define (par2 r1 r2)
  (div-interval one (add-interval (div-interval one r1) (div-interval one r2))))

(define r1 (make-interval 1024 1042))
(define r2 (make-interval 10004 10005))

(display (string-append
          "\npar1 result: "
          (interval->string (par1 r1 r2))
          "\npar2 result: "
          (interval->string (par2 r1 r2))
          "\nPercentage comparison: "
          (number->string (percent-interval (par1 r1 r2)))
          ", "
          (number->string (percent-interval (par2 r1 r2)))
          "\nDouble inversion: "
          (interval->string (div-interval one (div-interval one r1)))
          ", "
          (interval->string (div-interval one (div-interval one r2)))
          "\nMul intervals: "
          (interval->string (mul-interval r1 r2))
          "\nAdd intervals: "
          (interval->string (add-interval r1 r2))
          "\n"))
