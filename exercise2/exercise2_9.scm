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

;;; (= (+ (width-interval x) (width-interval y)) (width-interval (add-interval x y)))
;=> (=
;     (+ (/ (- (upper-bound x) (lower-bound x)) 2.0) (/ (- (upper-bound y) (lower-bound y)) 2.0))
;     (/ (- (upper-bound (add-interval x y)) (lower-bound (add-interval x y))) 2.0))
;=> (=
;     (+ (/ (- (upper-bound x) (lower-bound x)) 2.0) (/ (- (upper-bound y) (lower-bound y)) 2.0))
;     (/ (-
;         (upper-bound (make-interval (+ (lower-bound x) (lower-bound y)) (+ (upper-bound x) (upper-bound y))))
;         (lower-bound (make-interval (+ (lower-bound x) (lower-bound y)) (+ (upper-bound x) (upper-bound y)))))
;       2.0))
;=> (=
;     (+ (/ (- (max x) (min x)) 2.0) (/ (- (max y) (min y)) 2.0))
;     (/ (-
;         (max ((+ (min x) (min y)) (+ (max x) (max y))))
;         (min ((+ (min x) (min y)) (+ (max x) (max y)))))
;       2.0))
;;; NOTE: Rearrangment
;=> (=
;     (/ (+ (- (max x) (min x)) (- (max y) (min y))) 2.0)
;     (/ (- (+ (max x) (max y)) (+ (min x) (min y))) 2.0))
;=> (=
;     (/ (+ (+ (max x) (- (min x))) (+ (max y) (- (min y)))) 2.0)
;     (/ (+ (+ (max x) (max y)) (+ (- (min x)) (- (min y)))) 2.0))
;=> (=
;     (/ (- (+ (max x) (max y)) (+ (min x) (min y))) 2.0)
;     (/ (- (+ (max x) (max y)) (+ (min x) (min y))) 2.0))
;=> #t   ;;; QED
; Note: this proof also works for x<0 and y<0, we can simply flip the signs:
;;; (= (+ (width-interval x) (width-interval y)) (width-interval (sub-interval x y)))
;=> (=
;     (/ (- (+ (max x) (max -y)) (+ (min x) (min -y))) 2.0)
;     (/ (- (+ (max x) (max -y)) (+ (min x) (min -y))) 2.0))
; However, it is quite clear looking at this derivation that since the ORDER matters with
; multiplication and division that they will not be able to be reduced in the same way.
; Specifically, this derivation will fail at the noted point where rearrangement occurs.

(define x (make-interval 5 7))
(define y (make-interval 4 9))

(newline)
(display "Is width after `add-interval` additive? ")
(display (= (+ (width-interval x) (width-interval y)) (width-interval (add-interval x y)))) ; => #t
(newline)
(display "Is width after `sub-interval` additive? ")
(display (= (+ (width-interval x) (width-interval y)) (width-interval (sub-interval x y)))) ; => #t
(newline)
(display "Is width after `mul-interval` additive? ")
(display (= (+ (width-interval x) (width-interval y)) (width-interval (mul-interval x y)))) ; => #f
(newline)
(display "Is width after `div-interval` additive? ")
(display (= (+ (width-interval x) (width-interval y)) (width-interval (div-interval x y)))) ; => #f
(newline)

; If width after multiplication is only a function of width then if we move the intervals we should
; never get different results
(define offset (make-interval 5 5))
(define a (add-interval x offset))
(define b (add-interval y offset))

(display "Does moving intervals without changing width have same width after `add-interval`? ")
(display (= (width-interval (add-interval x y)) (width-interval (add-interval a b)))) ; => #t
(newline)
(display "Does moving intervals without changing width have same width after `sub-interval`? ")
(display (= (width-interval (sub-interval x y)) (width-interval (sub-interval a b)))) ; => #t
(newline)
(display "Does moving intervals without changing width have same width after `mul-interval`? ")
(display (= (width-interval (mul-interval x y)) (width-interval (mul-interval a b)))) ; => #f
(newline)
(display "Does moving intervals without changing width have same width after `div-interval`? ")
(display (= (width-interval (div-interval x y)) (width-interval (div-interval a b)))) ; => #f
(newline)

; Therefore: width after mult and div CANNOT be functions of only input widths
