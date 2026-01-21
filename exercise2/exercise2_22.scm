(define l (list 1 2 3 4))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) (cons (* (car things) (car things)) answer))))

  (iter items nil))

;;; This method reads the list head to tail, while building the result tail-to-head
;;; This is why it reverses the elements
(display (square-list l)) ; => (16 9 4 1)
(newline)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) (cons answer (* (car things) (car things))))))

  (iter items nil))

;;; This method doesn't build a list! A list would be:
;;;     (cons 1 (cons 2 (cons 3 (cons 4 '()))))
;;; This builds:
;;;     (cons (cons (cons (cons '() 1) 2) 3) 4)
;;; In scheme: a list is basically a tree with only leaf nodes on the left.
;;; This is a mirror image of that, so it doesn't fit what all of our builtin list procedures
;;; expect.
(display (square-list l)) ; = > ((((() . 1) . 4) . 9) .16)
(newline)

;;; ~~~ Correct code ~~~
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) (cons (* (car things) (car things)) answer))))

  (reverse (iter items nil)))

;;; In most cases: eversing a linked list is cheaper than inflating the stackframe with naive
;;; recursion. No need to panic, just reverse the list and call it a day.
(display (square-list l)) ; => (1 4 9 16)
(newline)
