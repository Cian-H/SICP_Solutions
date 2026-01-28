(car ''abracadabra) ; quote

; If we test without the `car` procedure we see that this '' double-quote appears to be further
; syntactic sugar that creates an empty quoted symbol with the default tag `'quote`

(begin ''abracadabra) ; (quote abracadabra)
(list 'quote 'abracadabra) ; (quote abracadabra)
(equal? (list 'quote 'abracadabra) ''abracadabra) ; #t
(equal? (list (quote quote) (quote abracadabra)) ''abracadabra) ; #t

; Given that it is common to use symbolic quotes prepended onto data to enforce data types in lisp,
; this begs the question: is `quote` just the following?

(define (quote x) (cons 'quote x))

; If so, we probably see this `quote` output because we're getting a structure like the following:

; ''abracadabra => (list 'quote (list 'quote abracadabra))

; And i would guess that normally, anything with the structure `(list 'quote x)` would just be
; displayed as `'x`. I can test this by redefining `quote` and testing if the above predicate still
; holds true:

(begin
  (define (quote x) (cons 'quote x))
  (equal? (list (quote quote) (quote abracadabra)) ''abracadabra)) ; abracadabra: undefined

; Of course! To do this inside the interpreter I'd need some way to tell the interpreter that the
; argument being given to `quote` doesn't need to exist and should be treated as a label for a
; memory address. I'm sure this can probably be done in a macro or similar, but this tells me
; `quote` is *almost certainly* a special form in scheme. HOWEVER, i still suspect that the special
; form is probably just tagging the data with a `quote` symbol under-the-hood. The only thing i
; still amn't sure about is how this special case avoids the infinite recursion that would be
; implied by the existence of a `'quote` label. My guess is that 'quote is probably a special case
; in the interpreter that creates a symbolic label that is loop breaking and atomic rather than
; being `(quote (quote (quote (quote...` onwards to infinity. Either way, i suspect there's not
; much more headway i can make probing this without a much more advanced level of scheme skill.
