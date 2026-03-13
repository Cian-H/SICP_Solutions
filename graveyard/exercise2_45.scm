;;; This exercise asks us to abstract out the general procedure for a `split`.
;;; HOWEVER: I notice that there is the generalised `split` it asks for (which generalises well for
;;; *ortholinear* splits, whereas the `corner-split` procedure also reveals a more generalised
;;; case for `split`. Therefore, i am going to deviate *slightly* from the exercise and create both
;;; abstractions at once.

(define (split transform-proc)
  (lambda (painter n)
    (if (= n 0)
      painter ; case 1: original is untouched
      (transform-proc painter (- n 1))))) ; case 2: 1 recursing inwards -> apply transform

(define (split-ortholinear transform1-proc transform2-proc)
  (define (inner painter n)
    (let ((smaller (inner painter (- n 1))))
      (transform1-proc painter (transform2-proc smaller smaller))))
  (split inner))

(define right-split (split-ortholinear beside below))
(define up-split (split-ortholinear below beside))
(define corner-split
  (split
    (lambda (painter n)
      (let* ((up (up-split painter (- n 1)))
             (right (right-split painter (- n 1)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1))))
        (beside (below painter top-left) (below bottom-right corner))))))

;;; This apparently will result in infinite recursion. It seems i'll need a *working*
;;; implementation of the picture language to have a chance at solving this one. For now, I'm going
;;; to give up on this experiment and come back to trying to figure it out once I have a working
;;; picture language.
