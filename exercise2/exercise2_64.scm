(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let* ((left-size (quotient (- n 1) 2))
           (left-result (partial-tree elts left-size))
           (left-tree (car left-result))
           (non-left-elts (cdr left-result))
           (right-size (- n (+ left-size 1)))
           (this-entry (car non-left-elts))
           (right-result (partial-tree (cdr non-left-elts) right-size))
           (right-tree (car right-result))
           (remaining-elts (cdr right-result)))
      (cons (make-tree this-entry left-tree right-tree) remaining-elts))))

;; Partial list works by splitting a list into a left sublist that is 1
;; element shorter than half the list, a single middle element, and the
;; remaining right (almost) half of the list. It then recursively calls itself
;; on the left and right sublists, and makes these subtrees the left and right
;; branches of a tree with the middle element as its root.

;; We expect the following tree:
;;           5
;;          / \
;;         1   9
;;          \  /\
;;          3 7 11

;; When we initially call `list->tree`, we get the length. For a linked list,
;; `length` requires us to walk the list from head to tail, which implies `n`
;; operations. As we walk the list, at each element: we create a 3 element list
;; representing the subtree for that element. This implies we call `list` n
;; times, for a total of `n` operations. The other operations performed are
;; expected to take negligible time. Therefore, we expect the runtime to be
;; approcimately `an + bn` where a is the time taken for each visit in the
;; `length` operation and `b` is the time taken for each `list` operation.
;; For big O notation, we remove these unknown constant variables, giving us a
;; big O of O(n) for the function `list->tree`.

(define l (list 1 3 5 7 9 11))

(display (list->tree l)) ; => (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
