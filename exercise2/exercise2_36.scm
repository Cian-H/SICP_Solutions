(load "lib/functools.scm")

(define (accumulate-n op acc seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op acc (map car seqs))
      (accumulate-n op acc (map cdr seqs)))))

(define t (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(newline)
(display (accumulate-n + 0 t))
(newline)
