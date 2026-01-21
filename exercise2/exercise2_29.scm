(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (left-length mobile)
  (branch-length (left-branch mobile)))

(define (right-length mobile)
  (branch-length (right-branch mobile)))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (if (pair? mobile)
    (+ (left-weight mobile) (right-weight mobile))
    mobile))

(define (left-weight mobile)
  (total-weight (branch-structure (left-branch mobile))))

(define (right-weight mobile)
  (total-weight (branch-structure (right-branch mobile))))

(define (left-torque mobile)
  (* (left-length mobile) (left-weight mobile)))

(define (right-torque mobile)
  (* (right-length mobile) (right-weight mobile)))

(define (balanced? mobile)
  (or (not (pair? mobile))
    (and (= (left-torque mobile) (right-torque mobile))
      (balanced? (branch-structure (left-branch mobile)))
      (balanced? (branch-structure (right-branch mobile))))))

;; Vibe coded test harness: why waste time writing tests for a path so well trodden?
;; ====================================================
;; TEST HARNESS FOR SICP EXERCISE 2.29
;; ====================================================

;; --- 1. Construct Test Data ---

;; A simple mobile: Left (len 10, wt 10), Right (len 10, wt 10)
;; Balanced? YES. Total Weight: 20
(define branch-a (make-branch 10 10))
(define branch-b (make-branch 10 10))
(define m1-simple-balanced (make-mobile branch-a branch-b))

;; A simple unbalanced mobile: Left (len 10, wt 10), Right (len 10, wt 5)
;; Balanced? NO. Total Weight: 15
(define branch-c (make-branch 10 5))
(define m2-simple-unbalanced (make-mobile branch-a branch-c))

;; A complex mobile (Nested):
;; Left Branch: Length 10, holds m1-simple-balanced (Weight 20). Torque = 200.
;; Right Branch: Length 40, holds Weight 5. Torque = 200.
;; Balanced? YES. Total Weight: 25
(define branch-nested-left (make-branch 10 m1-simple-balanced))
(define branch-nested-right (make-branch 40 5))
(define m3-nested-balanced (make-mobile branch-nested-left branch-nested-right))

;; A complex unbalanced mobile:
;; Same as above but changing the right length to 20.
;; Balanced? NO.
(define branch-nested-right-bad (make-branch 20 5))
(define m4-nested-unbalanced (make-mobile branch-nested-left branch-nested-right-bad))

;; --- 2. Run Tests ---

(display "--- Test 1: Simple Balanced Mobile ---")
(newline)
(display "Total Weight (Expect 20): ")
(display (total-weight m1-simple-balanced))
(newline)
(display "Balanced?    (Expect #t): ")
(display (balanced? m1-simple-balanced))
(newline)
(newline)

(display "--- Test 2: Simple Unbalanced Mobile ---")
(newline)
(display "Total Weight (Expect 15): ")
(display (total-weight m2-simple-unbalanced))
(newline)
(display "Balanced?    (Expect #f): ")
(display (balanced? m2-simple-unbalanced))
(newline)
(newline)

(display "--- Test 3: Nested Balanced Mobile ---")
(newline)
(display "Total Weight (Expect 25): ")
(display (total-weight m3-nested-balanced))
(newline)
(display "Balanced?    (Expect #t): ")
(display (balanced? m3-nested-balanced))
(newline)
(newline)

(display "--- Test 4: Nested Unbalanced Mobile ---")
(newline)
(display "Total Weight (Expect 25): ")
(display (total-weight m4-nested-unbalanced))
(newline)
(display "Balanced?    (Expect #f): ")
(display (balanced? m4-nested-unbalanced))
(newline)
(newline)
