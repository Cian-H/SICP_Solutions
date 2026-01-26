(load "lib/picture.scm")

;;; Testing harness
(define (test-impl label make get-origin get-edge1 get-edge2)
  (let* ((origin-vec (make-vect 1 2))
         (edge1-vec (make-vect 3 4))
         (edge2-vec (make-vect 5 6))
         (f (make origin-vec edge1-vec edge2-vec))
         (result-string (string-append
                         "\n~~~ "
                         label
                         "~~~\n\norigin: in = "
                         (vect->string origin-vec)
                         ", out = "
                         (vect->string (get-origin f))
                         "\nedge1: in = "
                         (vect->string edge1-vec)
                         ", out = "
                         (vect->string (get-edge1 f))
                         "\nedge2: in = "
                         (vect->string edge2-vec)
                         ", out = "
                         (vect->string (get-edge2 f))
                         "\n\n")))
    (display result-string)))

;;; Implementation 1 (list)
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin f)
  (car f))

(define (frame-edge1 f)
  (cadr f))

(define (frame-edge2 f)
  (caddr f))

(test-impl "Inplementation 1: list" make-frame frame-origin frame-edge1 frame-edge2)

;;; Implementation 2 (cons-chain)
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-origin f)
  (car f))

(define (frame-edge1 f)
  (cadr f))

(define (frame-edge2 f)
  (cddr f))

(test-impl "Implementation 2: cons-chain" make-frame frame-origin frame-edge1 frame-edge2)
