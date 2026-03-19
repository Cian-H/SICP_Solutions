;;; The new implementations work because the `complex` type wraps either the `polar` or
;;; `rectangular` type, as shown below.

(display (complex-from-real-imag 1 2)) ; => (complex rectangular 1 . 2)

;;; By dispatching the way that Alyssa suggests, `apply-generic` will unwrap the `complex` type
;;; exposing the subtype (either `polar` or `rectangular`). It will then dispatch to the
;;; `complex` package, which then calls `aply-generic` on the unwrapped type which dispatches the
;;; correct methods for that subtype. Basically: the type goes through the following changes:

(apply-generic 'real-part (complex-from-real-imag 1 . 2))
(apply-generic 'real-part '(complex rectangular 1 . 2))
(get 'real-part 'complex '(complex rectangular 1 . 2))
(apply-generic 'real-part '(rectangular 1 . 2))
(get 'real-part 'rectangular '(rectangular 1 . 2))
(car '(1 . 2))
1

;;; As we can see from this: the `apply-generic` procedure gets evaluated twice, invoking the
;;; '(real-part complex) method first, and then the '(real-part rectangular) method.
