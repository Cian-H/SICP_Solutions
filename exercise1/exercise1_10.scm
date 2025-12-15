(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1) (A x (- y 1))))))

; Expression 1
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

; Expression 2
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 1 (A 0 (A 1 3))))
(A 1 (A 1 (A 0 (A 0 (A 1 2)))))
(A 1 (A 1 (A 0 (A 0 (A 0 (A 1 1))))))
(A 1 (A 1 (A 0 (A 0 (A 0 2)))))
(A 1 (A 1 (A 0 (A 0 4))))
(A 1 (A 1 (A 0 8)))
(A 1 (A 1 16))
(A 1 (A 0 (A 1 15)))
(A 1 (A 0 (A 0 (A 1 14))))
(A 1 (A 0 (A 0 (A 0 (A 1 13)))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 1 12))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11)))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9)))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7)))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5)))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4))))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3)))))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4)))))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8))))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16)))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32))))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64)))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128))))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256)))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512))))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024)))))))
(A 1 (A 0 (A 0 (A 0 (A 0 (A 0 2048))))))
(A 1 (A 0 (A 0 (A 0 (A 0 4096)))))
(A 1 (A 0 (A 0 (A 0 8192))))
(A 1 (A 0 (A 0 16384)))
(A 1 (A 0 32768))
(A 1 65536) ; Oh dear god, no!
(A 0 (A 1 65535)) ; Will follow same pattern as Expression 1 from here, until (= y 1)
; ... ans = 2 ^ 65536

; NOTE: This pattern gives 2 ^ y when x is 1
;     Also, when x > 0 and y > 1 it seems to result in y ending up as 2 ^ (2 ^ y)
;     Therefore: the function result is 2 ^ (2 ^ (2 ^ 2))
;     I notice that the result when x >= 1 it expanded out into (A (- x 1) x') where x' was
;     effectively 2 ^ x. So, we expect this to result in (2 ^ x') ^ x'', and so on until x is 0.
;     So, i *think* we're seeing it end up as effectively 2 ^^ x. If that's the case then i'm
;     Guessing that the trend is that x is determining the level of the hyperoperation?
;     Next expression looks even bigger so: to hell with that, lets just do the maths here based
;     on this non-rigorous guess

; Expression 3
(A 3 3)
; ans = 2 ^^^ 3 = 2 ^ (2 ^ (2 ^ 2))

; Expression Definitions
(define (f n) (A 0 n)) ; 2 n
(define (g n) (A 1 n)) ; 2 ^ n
(define (h n) (A 2 n)) ; 2 ^^ n
(define (k n) (* 5 n n)) ; 5 * (n ^ 2)
