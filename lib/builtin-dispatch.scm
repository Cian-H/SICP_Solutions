(load "lib/safe-load.scm")
(load "lib/typing.scm")
(load "lib/dispatch-table.scm")

(define (install-builtin-dispatch)

  ;;; Register primitive types
  (register-type 'number)
  (register-type 'string)
  (register-type 'boolean)
  (register-type 'symbol)
  (register-type 'pair)
  (register-type 'null)

  ;;; ==========================================
  ;;; NUMBERS
  ;;; ==========================================

  ;; Basic Arithmetic
  (put 'add 'number 'number +)
  (put 'sub 'number 'number -)
  (put 'mul 'number 'number *)
  (put 'div 'number 'number /)

  ;; Predicates
  (put 'equal? 'number 'number =)
  (put 'zero? 'number zero?)

  ;; Complex Number Compatibility
  (put 'real-part 'number (lambda (x) x))
  (put 'imag-part 'number (lambda (x) 0))
  (put 'magnitude 'number abs)
  (put 'angle 'number (lambda (x) (if (>= x 0) 0 (acos -1.0)))) ; 0 or pi

  ;;; ==========================================
  ;;; STRINGS
  ;;; ==========================================

  (put 'add 'string 'string string-append)
  (put 'equal? 'string 'string string=?)
  (put 'length 'string string-length)

  ;;; ==========================================
  ;;; BOOLEANS
  ;;; ==========================================

  (put 'equal? 'boolean 'boolean eq?)
  (put 'invert 'boolean not)

  ;;; ==========================================
  ;;; SYMBOLS & PAIRS
  ;;; ==========================================

  (put 'equal? 'symbol 'symbol eq?)
  (put 'equal? 'pair 'pair equal?)
  (put 'equal? 'null 'null (lambda (x y) #t))

  'builtins-installed)

(install-builtin-dispatch)
