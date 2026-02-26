(define *loaded-modules* (cons "lib/safe-load.scm" '()))

(define (load file-path)
  (if (not (member file-path *loaded-modules*))
    (begin
      (set! *loaded-modules* (cons file-path *loaded-modules*))
      (load file-path))))
