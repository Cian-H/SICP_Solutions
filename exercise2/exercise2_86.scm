;;; The most straightforward way to achieve this seems to be to:
;;; - Change the package installs to add operations to the dispatch table with symbols matching
;;;     their corresponding builtins (e.g: `(put '+ 'rational 'rational add))
;;; - Alias the builtins (e.g: `(define scheme-add +)`)
;;; - Overload builtin with an apply-generic that falls back on the builtin if no generic is found
;;;
;;; Caveats to this approach:
;;; 1. Arity: We must match the function signatures of the original builtins. Native `+` is
;;;      variadic, meaning our generic `+` must fold/reduce over `n` arguments rather than just 2.
;;; 2. Infinite Loops: Any internal system procedures (like `apply-generic`, `drop`, or `length`)
;;;      MUST strictly use `scheme-add` instead of `+` to avoid recursive stack overflows.
;;; 3. Overhead: Every basic arithmetic operation in the program now incurs the cost of
;;;      tag checking and generic dispatch.
