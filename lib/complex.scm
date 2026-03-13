(load "lib/safe-load.scm")
(load "lib/typing.scm")
(load "lib/dispatch-table.scm")
(load "lib/math.scm")

(load "lib/complex/generic.scm")
(load "lib/complex/rectangular.scm")
(load "lib/complex/polar.scm")
(load "lib/complex/constructors.scm")
(load "lib/complex/accessors.scm")
(load "lib/complex/operations.scm")

(complex-install-rectangular-package)
(complex-install-polar-package)
