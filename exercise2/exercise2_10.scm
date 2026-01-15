(define (make-interval a b) (cons a b))

(define (predicate-selector f)
  (lambda (i)
    (let ((i0 (car i))
          (i1 (cdr i)))
      (if (f i0 i1) i0 i1))))

(define lower-bound (predicate-selector <))
(define upper-bound (predicate-selector >))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (add-interval
    x
    (make-interval
      (- (upper-bound y))
      (- (lower-bound y)))))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

; Wait a second: what actually happens if we divide by an interval spanning zero???
; Could logic it out, buts it's more fun to just test it
(define x (make-interval 5 7))
(define y (make-interval 4 9))
(define z (make-interval -2 4))

(newline)
(display "~~~ Test of original `div-interval` ~~~")
(newline)
(display "Test: normal `div-interval` -> (div-interval x y) => ")
(display (div-interval x y))
(newline)
(display "Test: undefined `div-interval` -> (div-interval x z) => ")
(display (div-interval x z))

; Huh, apparently this makes a negative interval due to the mult, and the size of the result
; definitely doesn't make sense for a "division"

(define (spans-zero? x)
  (<= (* (lower-bound x) (upper-bound x)) 0))

(define div-interval-old div-interval)
(define (div-interval x y)
  (if (spans-zero? y)
    (error "Interval y cannot span zero!")
    (div-interval-old x y)))

; Now lets repeat the tests
(newline)
(display "~~~ Test of fixed `div-interval` ~~~")
(newline)
(display "Test: normal `div-interval` -> (div-interval x y) => ")
(display (div-interval x y))
(newline)
(display "Test: undefined `div-interval` -> (div-interval x z) => ")
(display (div-interval x z))
