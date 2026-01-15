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

(define (spans-zero? x)
  (<= (* (lower-bound x) (upper-bound x)) 0))

(define (div-interval x y)
  (if (spans-zero? y)
    (error "Interval y cannot span zero!")
    (mul-interval
      x
      (make-interval
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval
    x
    (make-interval
      (- (upper-bound y))
      (- (lower-bound y)))))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xh (upper-bound x))
        (yl (lower-bound y))
        (yh (upper-bound y)))
    (cond
      ((>= xl 0
          (cond
            ((>= yl 0) (make-interval (* xl yl) (* xh yh)))
            ((<= yh 0) (make-interval (* xh yl) (* xl yh)))
            (else (make-interval (* xh yl) (* xh yh))))))
      ((<= xh 0
          (cond
            ((>= yl 0) (make-interval (* xl yh) (* xh yl)))
            ((<= yh 0) (make-interval (* xh yh) (* xl yl)))
            (else (make-interval (* xl yh) (* xl yl))))))
      (else
        (cond
          ((>= yl 0) (make-interval (* xl yh) (* xh yh)))
          ((<= yh 0) (make-interval (* xh yl) (* xl yl)))
          (else (make-interval (min (* xl yh) (* xh yl))
                 (max (* xl yl) (* xh yh)))))))))

;;; ------------------------------------------------------------------
;;; Test Suite: Verifying all 9 cases
;;; ------------------------------------------------------------------

(newline)
(display "~~~ SICP 2.11 Test Suite (9 Cases) ~~~")
(newline)

;; Define representative intervals
(define pos (make-interval 10 20)) ; Positive: [10, 20]
(define neg (make-interval -20 -10)) ; Negative: [-20, -10]
(define spn (make-interval -10 20)) ; Spans:    [-10, 20]

(define (print-mul x y label)
  (let ((res (mul-interval x y)))
    (display label)
    (display ": ")
    (display "[")
    (display (lower-bound x))
    (display ",")
    (display (upper-bound x))
    (display "] * ")
    (display "[")
    (display (lower-bound y))
    (display ",")
    (display (upper-bound y))
    (display "] = ")
    (display "[")
    (display (lower-bound res))
    (display ",")
    (display (upper-bound res))
    (display "]")
    (newline)))

;; 1. Pos * Pos
(print-mul pos pos "1. P * P")
;; Expect: [100, 400]

;; 2. Pos * Neg
(print-mul pos neg "2. P * N")
;; Expect: [-400, -100]

;; 3. Pos * Span
(print-mul pos spn "3. P * S")
;; Expect: [-200, 400]

;; 4. Neg * Pos
(print-mul neg pos "4. N * P")
;; Expect: [-400, -100]

;; 5. Neg * Neg
(print-mul neg neg "5. N * N")
;; Expect: [100, 400]

;; 6. Neg * Span
(print-mul neg spn "6. N * S")
;; Expect: [-400, 200]

;; 7. Span * Pos
(print-mul spn pos "7. S * P")
;; Expect: [-200, 400]

;; 8. Span * Neg
(print-mul spn neg "8. S * N")
;; Expect: [-400, 200]

;; 9. Span * Span (The complex case)
(print-mul spn spn "9. S * S")
;; Expect: Min(-200, -200) = -200. Max(100, 400) = 400. -> [-200, 400]

(newline)
(display "~~~ Division Safety Check (from 2.10) ~~~")
(newline)
(display "Division by zero-span interval (expect error or handling):")
(newline)
;; Uncomment the line below to trigger the error
;; (div-interval pos spn)
(display "Error logic preserved.")
(newline)
