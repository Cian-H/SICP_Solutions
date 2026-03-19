(load "lib/typing.scm")
(load "lib/dispatch-table.scm")
(load "lib/complex.scm")
(load "lib/rational.scm")

(define (apply-generic op . args)
  (let* ((types (map type-of args))
         (proc (get op types)))
    (if proc
      (apply proc (map type-unwrap args))
      (error "apply-generic: no method for types" (list op types)))))

(define (rational->number x)
  (/ (rational-numer x) (rational-denom x)))

(define (number->complex x)
  (complex-from-real-imag x 0))

(define (rational->complex x)
  (number->complex (rational->number x)))

(put 'equ? 'number 'number =)
(put 'equ? 'rational 'rational (get 'equal? 'rational 'rational)) ; see lib/rational for implementation
(put 'equ? 'complex 'complex (get 'equal? 'complex 'complex)) ; see lib/complex for implementation

(put 'equ? 'number 'rational
  (lambda (x y) (= x (rational->number (type-wrap 'rational y)))))

(put 'equ? 'rational 'number
  (lambda (x y) (equ? y (type-wrap 'rational x))))

(put 'equ? 'number 'complex
  (lambda (x y) (equ? (number->complex x) (type-wrap 'complex y))))

(put 'equ? 'complex 'number
  (lambda (x y) (equ? y (type-wrap 'complex x))))

(put 'equ? 'rational 'complex
  (lambda (x y) (equ? (rational->complex (type-wrap 'rational x))
                 (type-wrap 'complex y))))

(put 'equ? 'complex 'rational
  (lambda (x y) (equ? (type-wrap 'rational y)
                 (type-wrap 'complex x))))

(define (equ? x y)
  (apply-generic 'equ? x y))

;; ==========================================
;; TEST SUITE: Cross-Type Equality (equ?)
;; ==========================================

;; A simple test helper
(define (run-test name expected actual)
  (display (string-append name ": "))
  (if (eq? expected actual)
    (display "\033[32mPASS\033[0m\n")
    (begin
      (display "\033[31mFAIL\033[0m (Expected ")
      (display expected)
      (display ", got ")
      (display actual)
      (display ")\n"))))

;; Define test values that are mathematically equivalent to 2
(define num-2 2)
(define rat-2 (rational 4 2)) ; Reduces to 2/1
(define com-2 (complex-from-real-imag 2 0))

;; Define test values that are mathematically equivalent to 0.5
(define num-half 0.5)
(define rat-half (rational 1 2))
(define com-half (complex-from-real-imag 0.5 0))

(display "\n--- Testing TRUE Conditions (Values = 2) ---\n")
;; Same Type
(run-test "Number   == Number  " #t (equ? num-2 2))
(run-test "Rational == Rational" #t (equ? rat-2 (rational 2 1)))
(run-test "Complex  == Complex " #t (equ? com-2 (complex-from-real-imag 2 0)))

;; Cross Type (Number & Rational)
(run-test "Number   == Rational" #t (equ? num-2 rat-2))
(run-test "Rational == Number  " #t (equ? rat-2 num-2))

;; Cross Type (Number & Complex)
(run-test "Number   == Complex " #t (equ? num-2 com-2))
(run-test "Complex  == Number  " #t (equ? com-2 num-2))

;; Cross Type (Rational & Complex)
(run-test "Rational == Complex " #t (equ? rat-2 com-2))
(run-test "Complex  == Rational" #t (equ? com-2 rat-2))

(display "\n--- Testing FALSE Conditions (2 vs 0.5) ---\n")
;; Same Type
(run-test "Number   != Number  " #f (equ? num-2 num-half))
(run-test "Rational != Rational" #f (equ? rat-2 rat-half))
(run-test "Complex  != Complex " #f (equ? com-2 com-half))

;; Cross Type (Number & Rational)
(run-test "Number   != Rational" #f (equ? num-2 rat-half))
(run-test "Rational != Number  " #f (equ? rat-2 num-half))

;; Cross Type (Number & Complex)
(run-test "Number   != Complex " #f (equ? num-2 com-half))
(run-test "Complex  != Number  " #f (equ? com-2 num-half))

;; Cross Type (Rational & Complex)
(run-test "Rational != Complex " #f (equ? rat-2 com-half))
(run-test "Complex  != Rational" #f (equ? com-2 rat-half))

(display "\nTests complete.\n")
