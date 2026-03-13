(define (predicate-selector f)
  (lambda (i)
    (let ((i0 (car i))
          (i1 (cdr i)))
      (if (f i0 i1) i0 i1))))

(define lower-bound (predicate-selector <))
(define upper-bound (predicate-selector >))

(define (sort-interval x) (cons (lower-bound x) (upper-bound x)))
(define (make-interval a b) (sort-interval (cons a b)))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (spans-zero? x)
  ((if (< (lower-bound x) 0) > <) (upper-bound x) 0))

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

; Ok, to start: what are the 9 cases? Each number can have 1 of 3 signs, so lets make a matrix
; of interval states. For my implementation, where the intervals MUST be sorted properly, that
; gives us the following grid:
;
;       ||   -   |   0   |   +
;   =============================
;    -  ||  -/-  |  -/0  |  -/+
;    0  ||  0/-  |  0/0  |  0/+
;    +  ||  +/-  |  +/0  |  +/+
;
; Ok, now let's get the Kronecker square of that grid since we're multiplying pairs:
;   Note: let component is on the vertical, and right component is on the horizontal.
;
;        || -/- | -/0 | -/+ || 0/0 | 0/+ || +/+
;   ============================================
;    -/- || +/+ | 0/+ | -/+ || 0/0 | -/0 || -/-
;    -/0 || 0/+ | 0/+ | -/+ || 0/0 | -/0 || -/0
;    -/+ || -/+ | -/+ | -/+ || 0/0 | -/+ || -/+
;   --------------------------------------------
;    0/0 || 0/0 | 0/0 | 0/0 || 0/0 | 0/0 || 0/0
;    0/+ || -/0 | -/0 | -/+ || 0/0 | 0/+ || 0/+
;   --------------------------------------------
;    +/+ || -/- | -/0 | -/+ || 0/0 | 0/+ || +/+
;
; This grid is clearly centrally symmetrical table, which makes sense for multiplication.
; This means we can simplify the table down as follows:
;
;        || +/+ | +/0 | +/- | 0/+ | 0/0
;   ====================================
;    +/+ || +/+ | +/0 | +/- | 0/+ | 0/0
;    +/0 || +/0 | +/0 | +/0 | 0/0 | 0/0
;    +/- || +/- | +/0 | -/+ | 0/+ | 0/0
;    0/+ || 0/+ | 0/0 | 0/+ | 0/+ | 0/0
;    0/0 || 0/0 | 0/0 | 0/0 | 0/0 | 0/0
;
; For my specific implementation, the arrangements where the elements are unsorted should never
; occur. This means we can further simplify the grid:
;
;       | +/+ | 0/+ | 0/0 | -/+ | -/0 | -/-
; ==========================================
;  +/+  | +/+ | 0/+ | 0/0 | -/+ | -/0 | -/-  (Identity behavior, 2 mults at corners, 3 where mixed)
;  0/+  | 0/+ | 0/+ | 0/0 | -/+ | -/0 | -/0  (Left zero, max 1 mult based on other sign)
;  0/0  | 0/0 | 0/0 | 0/0 | 0/0 | 0/0 | 0/0  (Zero interval, 0 mults)
;  -/+  | -/+ | -/+ | 0/0 | -/+ | -/+ | -/+  (Mixed unless 0, 4 mults)
;  -/0  | -/0 | -/0 | 0/0 | -/+ | 0/+ | 0/+  (Right zero, max 1 mult based on other sign)
;  -/-  | -/- | -/0 | 0/0 | -/+ | 0/+ | +/+  (Sign flip, same as identity)
;
; Here, we see 16 total configurations of the system, with only 6 relevant cases (suck on that
; Ben Bitdiddle!). The relevant cases for control flow are (with predicate names, and in order of
; priority):
;
;   - Zero interval: 0 mults (`zero-interval?` on either, or could handle zero on sides separately)
;   - One side zero: 1 mult (`left-zero?`, `right-zero?` on either)
;   - Identity behaviour where mixed: 3 mults (`mixed?`, i.e: xor `spans-zero?`)
;   - Corners, internally matching signs in intervals: 2 mults (`(lambda (x) (not (spans-zero? x)))`)
;   - Fallthrough: 4 mults (`else`)

(define (xor a b)
  (if a (not b) b))

(define (left-zero? x)
  (zero? (car x)))

(define (right-zero? x)
  (zero? (cdr x)))

(define (mixed? x y)
  (xor (spans-zero? x) (spans-zero? y)))

(define (mul-interval x y)
  (define p1 (let ((r #f)) (lambda () (if (not r) (set! r (* (lower-bound x) (lower-bound y)))) r)))
  (define p2 (let ((r #f)) (lambda () (if (not r) (set! r (* (lower-bound x) (upper-bound y)))) r)))
  (define p3 (let ((r #f)) (lambda () (if (not r) (set! r (* (upper-bound x) (lower-bound y)))) r)))
  (define p4 (let ((r #f)) (lambda () (if (not r) (set! r (* (upper-bound x) (upper-bound y)))) r)))

  (define (fallthrough f)
    (f (p1) (p2) (p3) (p4)))

  (define (handle-mixed)
    (let* ((x-span (spans-zero? x))
           (spanning (if x-span x y))
           (not-spanning (if x-span y x))
           (not-spanning-larger ((if (negative? (lower-bound not-spanning)) lower-bound upper-bound) not-spanning))
           (larger-times-larger (* not-spanning-larger (upper-bound spanning))))
      (make-interval
        (min larger-times-larger (* (upper-bound not-spanning) (lower-bound spanning)))
        (max larger-times-larger (* not-spanning-larger (lower-bound spanning))))))

  (define (handle-unmixed-side x0 x1 y0 y1)
    (let ((is-zero (or (zero? x1) (zero? y1)))
          (is-right (> x0 x1))
          (neg-x (negative? x0))
          (neg-y (negative? y0)))
      (cond
        (is-zero 0)
        ((and neg-x neg-y is-right) (p4))
        ((and neg-x neg-y) (p1))
        (neg-y (p2))
        (neg-x (p3))
        (is-right (p1))
        (else (p4)))))

  (define (handle-unmixed)
    (cond
      ((and (spans-zero? x) (spans-zero? y)) (make-interval (min (p1) (p2)) (p4)))
      (else (let* ((xl (lower-bound x))
                   (xr (upper-bound x))
                   (yl (lower-bound y))
                   (yr (upper-bound y)))
             (make-interval
               (handle-unmixed-side xr xl yr yl)
               (handle-unmixed-side xl xr yl yr))))))

  (if (mixed? x y)
    (handle-mixed)
    (handle-unmixed)))
