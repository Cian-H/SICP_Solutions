; Instead of manually encoding a tree (yet again) lets just do this programatically and see what
; kind of tree we get!

(load "lib/functools.scm")
(load "lib/huffman.scm")

(define alphabet "abcdefghijklmnopqrstuvwxyz")

(define (construct-alphabet n)
  (define (rec reps l)
    (if (null? l)
      l
      (append (repeat (car l) reps) (rec (* 2 reps) (cdr l)))))
  (list->string (rec 1 (take (string->list alphabet) n))))

(define bar-char #\=)
(define bar-width 96)

(display (list->string (repeat bar-char bar-width)))
(newline)
(display (string->enctree (construct-alphabet 5)))
(newline)
(display (list->string (repeat bar-char bar-width)))
(newline)
(display (string->enctree (construct-alphabet 10)))
(newline)
(display (list->string (repeat bar-char bar-width)))
(newline)

;;; n=5 tree
;
; ((e d c a b) . 31)
; ├── ((d c a b) . 15)
; │   ├── ((c a b) . 7)
; │   │   ├── ((a b) . 3)
; │   │   │   ├── (a . 1)
; │   │   │   └── (b . 2)
; │   │   └── (c . 4)
; │   └── (d . 8)
; └── (e . 16)
;
; Most frequent letter (e) encoded by 1 bit
; Least frequent letter (a) encoded by 4 bits
;
;;; n=10 tree
;
; ((j i h g f e d c a b) . 1023)
; ├── ((i h g f e d c a b) . 511)
; │   ├── ((h g f e d c a b) . 255)
; │   │   ├── ((g f e d c a b) . 127)
; │   │   │   ├── ((f e d c a b) . 63)
; │   │   │   │   ├── ((e d c a b) . 31)
; │   │   │   │   │   ├── ((d c a b) . 15)
; │   │   │   │   │   │   ├── ((c a b) . 7)
; │   │   │   │   │   │   │   ├── ((a b) . 3)
; │   │   │   │   │   │   │   │   ├── (a . 1)
; │   │   │   │   │   │   │   │   └── (b . 2)
; │   │   │   │   │   │   │   └── (c . 4)
; │   │   │   │   │   │   └── (d . 8)
; │   │   │   │   │   └── (e . 16)
; │   │   │   │   └── (f . 32)
; │   │   │   └── (g . 64)
; │   │   └── (h . 128)
; │   └── (i . 256)
; └── (j . 512)
;
; Most frequent letter (j) encoded by 1 bit
; Least frequent letter (a) encoded by 9 bits
;
; The conclusion here seems obvious though, even without drawing the tree. If no 2 values share a
; weight then this algorithm will basically produce a linked list (or a structure that can be
; flattened to one) where each bit is basically a yes/no gate in a series of "is it this letter?"
; questions. Basically, this tree (for n=5) is the functional equivalent of:
;
; (define (decode-letter bit-list)
;   (define (iter bit-list letters)
;     (if (zero? (car bit-list))
;       (car bit-list)
;       (iter (cdr bit-list) (cdr letters))))
;   (iter bit-list (list "e" "d" "c" "b" "a")))
;
; As an interesting contrast to this if we use the following to get a tree where every letter is
; weighted equally (where n=8) we see a balanced tree emerge:
;
; (string->enctree (list->string (take (string->list alphabet) 8)))
;
; ((a b c d e f g h) . 8)
; ├── ((a b c d) . 4)
; │   ├── ((a b) . 2)
; │   │   ├── (a . 1)
; │   │   └── (b . 1)
; │   └── ((c d) . 2)
; │       ├── (c . 1)
; │       └── (d . 1)
; └── ((e f g h) . 4)
;     ├── ((e f) . 2)
;     │   ├── (e . 1)
;     │   └── (f . 1)
;     └── ((g h) . 2)
;         ├── (g . 1)
;         └── (h . 1)
;
; And this tree is perfectly analogous to the *fixed-length* encoded representation. The fact that
; fixed-length encoding and completely unary encoding are just special cases of Huffmann encoding
; is interesting, and since both are suboptimal suggests there should be a point on the spectrum
; where there is a "right" amount of variable length encoding that is optimal. I wonder where that
; is, if it is a fixed point, and if there are ways to reliably tokenize languages so that they
; encode to a tree with this "optimal" level of balance?
