(define (install-symbolic/parse-package)
  (load "lib/symbolic/parse/infix.scm")
  (load "lib/symbolic/parse/prefix.scm")
  (load "lib/symbolic/parse/postfix.scm")

  (install-symbolic/parse/infix-package)
  (install-symbolic/parse/prefix-package)
  (install-symbolic/parse/postfix-package)
  'ok)

(load "lib/symbolic/parse/generic.scm")
