(load "lib/functools.scm")
(load "lib/bstree.scm")

(define (key-getter kv) (car kv))
(define (val-getter kv) (cdr kv))

(define (key-compare pred kv1 kv2)
  (pred (key-getter kv1) (key-getter kv2)))

(define (key-lt kv1 kv2) (key-compare < kv1 kv2))

(define (lookup k tree)
  (let ((result (bstree-lookup (cons k '()) key-getter tree)))
    (if result (val-getter result) #f)))

(define kv-tree
  (apply bstree
    (cons key-lt
      (map
        (lambda (i) (cons i (string-append "Value " (number->string i))))
        (range 0 1000)))))

(display (lookup 512 kv-tree))
(newline)
