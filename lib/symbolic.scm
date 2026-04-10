(load "lib/safe-load.scm")
(load "lib/functools.scm")
(load "lib/typing.scm")
(load "lib/generic-dispatch.scm")
(load "lib/builtin-dispatch.scm")

(define (install-symbolic-package)
  (load "lib/symbolic/constants.scm")
  (load "lib/symbolic/parse.scm")
  (load "lib/symbolic/simplify.scm")
  (load "lib/symbolic/calculus.scm")

  (install-symbolic/constants-package)
  (install-symbolic/parse-package)
  (install-symbolic/simplify-package)
  (install-symbolic/calculus-package)

  'ok)

(install-symbolic-package)

(load "lib/symbolic/calculus/generic.scm")
(load "lib/symbolic/parse/generic.scm")
(load "lib/symbolic/simplify/generic.scm")
(load "lib/symbolic/generic.scm")
