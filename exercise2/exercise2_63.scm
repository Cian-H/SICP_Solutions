(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

; Method 1: The recursive way
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append
      (tree->list-1 (left-branch tree))
      (cons
        (entry tree)
        (tree->list-1 (right-branch tree))))))

; Method 2: The iterative way
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list
        (left-branch tree)
        (cons
          (entry tree)
          (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; ==========================================
;; Predictions
;; ==========================================
;; Method 1 is doubly recursive and uses `append` so i immediately expect it to
;; perform worse than Method 2, which is *partially* tail recursive and only
;; uses `cons`. The callstack for Method 2 should grow in size more slowly than
;; Method 1 because of this.
;;
;; The big O notation for method 1 is based on the fact that a balanced tree
;; has log_2(n) levels, and at each level we perform an append. At each level,
;; we split the tree into 2 branches, each of which makes a call to `append`.
;; The root has 1 appends and 1 node, layer 1 has 2 appends and 2, and so on.
;; In each of these appends: n will walk 1 of the 2 lists provided to it from
;; head to tail, as this is the only way to find the last node in a linked
;; list. Therefore, each append does `n/2` operations. Combining these, we
;; expect to have `log_2(n)*(n/2)`. However, since the length of time taken
;; by each operation is variable, we generally reduce this to the big O time
;; complexity by simply disregarding the constants, giving us a big O of
;; O(nlogn).
;;
;; As for method 2: it touches each node in the tree once and uses `cons` to
;; prepend it to the list. This implies we do n operations, giving us a big O
;; of `O(n)` for method 2.
;;
;; Therefore: big O suggests method 2 will grow more slowly.
;;
;; As for checking whether both methods are
;; equivalent, we should be able to do this with substitutional algebra and
;; inductive reasoning (in Polish notation to maintain clarity without the need
;; to context switch away from thinking in lisp).
;;
;; T=tree-node, E=entry, L=left-branch, R=right-branch, A=accumulator
;;
;; To prove equivalence, we want to prove that for ANY tree T, the following
;; hypothesis holds:
;;    (= (tree->list-1 T)
;;       (tree->list-2 T))
;;
;; To prove this hypothesis holds, we need to evaluate tree-list-2. Because
;; `(tree-list-2 T)` is defined as (copy-to-list T '()), this implies that our
;; hypothesis is equivalent to the lemma:
;;    (= (tree->list-1 T)
;;       (copy-to-list T '()))
;;
;; However, this lemma is extremely specific and dependent on the accumulator.
;; We *know* that the results of previous iterations must accumulate SOMEWHERE
;; as we evaluate `tree->list-1`. Since we know the base case `(= T '())` must
;; exist we can use this to hypothesise where this accumulator appears in
;; subsequent recursions.
;;    (tree->list-1 T)
;; => (append (tree->list-1 T_n) '()) ;
;; => (append (tree->list-1 T_n-1) (append (tree->list-1 T_n) '()))
;; Based on this, I hypothesise that subsequent appends accumulate in the tail
;; for this function.
;; => (append (tree->list-1 T_n-1) A)
;; We can use this to hypothesis to derive another lemma that generalises the
;; first lemma.
;;
;; Therefore, by the rules of inductive logic: to prove the original
;; hypothesis, we must first prove the following generalized Lemma:
;;    (= (append (tree->list-1 T) A)
;;       (copy-to-list T A))
;;
;; Therefore, if we can prove this lemma, we prove our hypothesis.
;;
;; === EXPANSION 1 ===
;; To start with, we expand the first expression (bottom up, from the base case):
;;    (append (tree->list-1 T) A_n) ; Recursion n (where n decrements)
;; => (append (append (tree->list-1 L) (cons E (tree->list-1 R))) A_n) ; +1 Recursion
;; We know that append is associative:
;;    (= (append (append X Y) Z)
;;       (append X (append Y Z)))
;; Therefore:
;; => (append (tree->list-1 L) (append (cons E (tree->list-1 R)) A))
;; We also know that appending a number to the head of a list is the same as
;; consing it with that list (below: where X is a value and Y/Z are lists)
;;    (= (append (cons X Y) Z)
;;       (cons X (append Y Z))
;; Therefore:
;; => (append (tree->list-1 L) (cons E (append (tree->list-1 R) A)))
;;
;; === EXPANSION 2 ===
;; Then, we expand the second expression:
;;    (copy-to-list T '())
;; => (copy-to-list T A_1) ; Recursion 1
;; => (copy-to-list L (cons E (copy-to-list R A_n))) ; Recursion n (where n increments)
;; Here, if the hypothesis holds we can inductively apply our generalised lemma:
;; => (copy-to-list L (cons E (append (tree->list-1 R) A_n)))
;; => (append (tree->list-1 L) (cons E (append (tree->list-1 R) A_n)))
;;
;; Substituting these expansions back into the original expression:
;;    (= (tree->list-1 T)
;;       (tree->list-2 T))
;; => (= (tree->list-1 T)
;;       (copy-to-list T '()))
;; => (= (append (tree->list-1 T) A)
;;       (copy-to-list T A))
;; => (= (append (append (tree->list-1 L) (cons E (tree->list-1 R))) A)
;;       (append (append (tree->list-1 L) (cons E (tree->list-1 R))) A))
;; => #t
;; QED

(define tree-1
  (make-tree 7
    (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
    (make-tree 9 '() (make-tree 11 '() '()))))

(define tree-2
  (make-tree 3
    (make-tree 1 '() '())
    (make-tree 7 (make-tree 5 '() '())
      (make-tree 9 '() (make-tree 11 '() '())))))

(define tree-3
  (make-tree 5
    (make-tree 3 (make-tree 1 '() '()) '())
    (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))

(define n 1)

(define (test t)
  (let ((n-string (number->string n)))
    (begin
      (display (string-append "Tree " n-string " with Method 1: "))
      (display (tree->list-1 t))
      (newline)
      (display (string-append "Tree " n-string " with Method 2: "))
      (display (tree->list-2 t))
      (newline)
      (set! n (+ n 1)))))

(map test (list tree-1 tree-2 tree-3))

;; Output =>
; Tree 1 with Method 1: (1 3 5 7 9 11)
; Tree 1 with Method 2: (1 3 5 7 9 11)
; Tree 2 with Method 1: (1 3 5 7 9 11)
; Tree 2 with Method 2: (1 3 5 7 9 11)
; Tree 3 with Method 1: (1 3 5 7 9 11)
; Tree 3 with Method 2: (1 3 5 7 9 11)
