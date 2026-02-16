(load "lib/functools.scm")

; Originally, i thought it was asking for a binary tree here, then i realised the next section
; explains btrees from scratch! So, i'm redoing this one with a binary search, which i'm guessing
; is probably what they *actually* wanted here.

; If that *is* what they wanted: then adjoining in one efficient pass becomes pretty trivial
; provided the lists are *already* sorted. Thankfully, i already did some playing around and
; implemented a basic library of sorts when getting to grips with scheme idioms earlier.
(load "lib/sort.scm")

(define (set . l)
  (sort-ascending l))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))

(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (car set)) #t)
    (else
      (let* ((halves (split-halves set))
             (left (car halves))
             (right (cdr halves)))
        (if (null? right)
          #f
          (let ((mid (car right)))
            (cond
              ((= x mid) #t)
              ((< x mid) (element-of-set? x left))
              (else (element-of-set? x (cdr right))))))))))
