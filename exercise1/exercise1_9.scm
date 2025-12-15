; Prelude
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

; Implementation 1
; Original
(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))

; Peano Expansion (my own mental model)
(define (+ a b)
  (if (= a 0)
    b
    (inc (if
          (= (dec a) 0)
          b
          (inc (+ (dec (dec a)) b)))))) ; `+` inside expression, not tail-recursive

; Concrete, Substitutive Expansion (SICP's mental model)
(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

; Shape: recursive, according to SICP criteria

; Implementation 2
; Original
(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

; Peano Expansion (my own mental model)
(define (+ a b)
  (if (= a 0)
    b
    (+
      (if (= a 0)
        b
        (+ (dec (dec a)) (inc (inc b))))))) ; `+` is outermost procedure, is tail-recursive

; Concrete, Substitutive Expansion (SICP's mental model)
(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9

; Shape: iterative, according to SICP criteria
