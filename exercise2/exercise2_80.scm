(load "exercise2/exercise2_79.scm")

;;; Honestly, i think this one should be solved as follows
(define (=zero? x) (equ? x 0))

(define (test zero-proc label)
  (display "\n==========================================\n")
  (display (string-append "   TEST SUITE: =zero? Implementation (" label ")\n"))
  (display "==========================================\n")

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

  (display "\n--- Testing TRUE Conditions (Values = 0) ---\n")
  (run-test "Number 0       " #t (zero-proc 0))
  (run-test "Number 0.0     " #t (zero-proc 0.0))
  (run-test "Rational 0/1   " #t (zero-proc (rational 0 1)))
  (run-test "Rational 0/5   " #t (zero-proc (rational 0 5))) ; Edge case
  (run-test "Complex 0+0i   " #t (zero-proc (complex-from-real-imag 0 0)))
  (display "\n--- Testing FALSE Conditions (Values != 0) ---\n")
  (run-test "Number 1       " #f (zero-proc 1))
  (run-test "Number -0.5    " #f (zero-proc -0.5))
  (run-test "Rational 1/2   " #f (zero-proc (rational 1 2)))
  (run-test "Complex 1+0i   " #f (zero-proc (complex-from-real-imag 1 0)))
  (run-test "Complex 0+1i   " #f (zero-proc (complex-from-real-imag 0 1)))
  (run-test "Complex 1+1i   " #f (zero-proc (complex-from-real-imag 1 1)))
  (display (string-append "\nTests (" label ") complete.\n")))

(test =zero? "wrap-generic")

;;; But, apparently, that's not the point of the exercise. So let's do this "properly".
;;; That being said: I disagree with SICP's definition of "properly" here, so if i'm gonna
;;; implement this on a type-by-type basis lets *at least* place the abstraction barriers
;;; in the right places (inside the modules for those types!)

(load "lib/typing.scm")
(load "lib/dispatch-table.scm")
(load "lib/complex.scm")
(load "lib/rational.scm")

; For plain numbers, lets borrow scheme's standard `zero?` predicate
(put '=zero? 'number zero?)
; For complex numbers, we get the dispatched `zero?` for that type:
;   - For rectangular: check if real and imag are both zero
;   - For polar: check if magnitude is zero
(put '=zero? 'complex (get 'zero? 'complex))
; Finally, for rationals, dispatch the `zero?` for that type:
;   - Checks if the numerator is 0
(put '=zero? 'rational (get 'zero? 'rational))

(define (=zero? x) (apply-generic '=zero? x))

(test =zero? "direct-generic")
