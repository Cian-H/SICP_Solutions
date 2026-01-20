(define (make-interval a b) (cons a b))

(define (predicate-selector f)
  (lambda (i)
    (let ((i0 (car i))
          (i1 (cdr i)))
      (if (f i0 i1) i0 i1))))

(define lower-bound (predicate-selector <))
(define upper-bound (predicate-selector >))

(define (interval->string i)
  (let ((i0 (number->string (lower-bound i)))
        (i1 (number->string (upper-bound i))))
    (string-append "[" i0 ", " i1 "]")))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

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

(define (pos-or-zero? x)
  (not (negative? x)))

(define (neg-or-zero? x)
  (not (positive? x)))

(define (mul-interval x y)
  (let ((x0 (lower-bound x))
        (x1 (upper-bound x))
        (y0 (lower-bound y))
        (y1 (upper-bound y)))
    (cond
      ((pos-or-zero? x0) (cond
                           ((pos-or-zero? y0) (make-interval (* x0 y0) (* x1 y1)))
                           ((neg-or-zero? y1) (make-interval (* x1 y0) (* x0 y1)))
                           (else (make-interval (* x1 y0) (* x1 y1)))))
      ((neg-or-zero? x1) (cond
                           ((pos-or-zero? y0) (make-interval (* x0 y1) (* x1 y0)))
                           ((neg-or-zero? y1) (make-interval (* x1 y1) (* x0 y0)))
                           (else (make-interval (* x0 y1) (* x0 y0)))))
      (else (cond
              ((pos-or-zero? y0) (make-interval (* x0 y1) (* x1 y1)))
              ((neg-or-zero? y1) (make-interval (* x1 y0) (* x0 y0)))
              (else (make-interval (min (* x0 y1) (* x1 y0)) (max (* x0 y0) (* x1 y1)))))))))

;;; ------------------------------------------------------------------
;;; Test Suite: Verifying all 9 cases
;;; ------------------------------------------------------------------

(newline)
(display "~~~ SICP 2.11 Test Suite (9 Cases) ~~~")
(newline)

;; Define representative intervals
(define pos (make-interval 10 20))   ; Positive: [ 10,  20]
(define neg (make-interval -20 -10)) ; Negative: [-20, -10]
(define spn (make-interval -10 20))  ; Spans:    [-10,  20]

(define (print-op f op-label)
  (lambda (x y label)
    (let* ((z (f x y))
           (xs (interval->string x))
           (ys (interval->string y))
           (zs (interval->string z)))
      (display (string-append label ": " xs " " op-label " " ys " = " zs))
      (newline))))

(define print-mul (print-op mul-interval "*"))
(define print-div (print-op div-interval "/"))

(print-mul pos pos "1. P * P") ; [100, 400]
(print-mul pos neg "2. P * N") ; [-400, -100]
(print-mul pos spn "3. P * S") ; [-200, 400]
(print-mul neg pos "4. N * P") ; [-400, -100]
(print-mul neg neg "5. N * N") ; [100, 400]
(print-mul neg spn "6. N * S") ; [-400, 200]
(print-mul spn pos "7. S * P") ; [-200, 400]
(print-mul spn neg "8. S * N") ; [-400, 200]
(print-mul spn spn "9. S * S") ; [-200, 400]

(newline)
(display "~~~ Division Safety Check (from 2.10) ~~~")
(newline)
(print-div pos pos "1. P / P")
(print-div pos neg "2. P / N")
(print-div neg pos "3. N / P")
(print-div neg neg "4. N / N")
(display "Division by zero-span interval (expect error: \"Interval y cannot span zero!\"):")
(newline)
(print-div pos spn "5. P / S")
