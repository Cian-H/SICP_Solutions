(define l1 (list 1 (list 2 (list 3 4))))

(newline)

;;; For below, before executing: I expect it to print `(1 2 3 4)`
(display l1) ; => (1 (2 (3 4)))
(newline)

;;; Interesting! Why is it being displayed as nested `cons` pairs?
;;; Could it be because it's not terminated with an empty list?

(define l2 (list 1 (list 2 (list 3 4 '()))))
(display l2) ; => (1 (2 (3 4 ())))
(newline)

;;; Ok, i am VERY intrigued now! Why is it doing this???
;;; Below are box-and-pointer plots of a flat list and this to compare:

;;; ~~~ `(list 1 2 3 4)` ~~~

; [.|.]>[.|.]>[.|.]>[.|.]>[.|/]
;  v     v     v     v     v
; [1]   [2]   [3]   [4]  [()]

;;; ~~~ `(list 1 (list 2 (list 3 4)))` ~~~

; [.|.]>[.|.]>[.|/]
;  v     |     v
; [1]    |   [()]
;        v
;       [.|.]>[.|.]>[.|/])
;       v     |     v
;      [2]    |   [()]
;             v
;            [.|.]>[.|.]>[.|/]
;            v     v      v
;           [3]   [4]   [()]

;;; Of course! I considered that maybe the issue was that i could be *MISSING* the empty list, but
;;; I forgot that every invocation of `list` adds an empty list as a leaf on the right and branches
;;; the new list off to the LEFT. Lists only accumulate to the right, so this is why i'm seeing
;;; this weirdness. The tree data structures clearly demonstrate this difference:

;;; ~~~ `(list 1 2 3 4)` ~~~

;   .
;  / \
; 1   .
;    / \
;   2   .
;      / \
;     3   .
;        / \
;       4   .
;          / \
;         5  ()

;;; ~~~ `(list 1 (list 2 (list 3 4)))` ~~~

;   .
;  / \
; 1   .
;    / \
;   .  ()
;  / \
; 2   .
;    / \
;   .  ()
;  / \
; 3   .
;    / \
;   4  ()

;;; These trees are clearly VERY different. The bottom one is obviously not a list.

;;; For the sake of completeness: lets also include the abstract version of the tree

;        (root)
;       /      \
;     1       (2 (3 4))
;            /         \
;           2        (3 4)
;                   /     \
;                  3       4

;;; Finally, given this undersanding, let's try to manually construct this tree to verify that it
;;; matches what we see.
(define l3 (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 '())) '())) '())))
(display l3) ; => (1 (2 (3 4)))
(newline)
(display (if (equal? l1 l3) "\033[32mSUCCESS!\033[0m\n" "\033[31mFAILURE...\033[0m\n")) ; => SUCCESS!
(newline)

;;; The manually built tree is a perfect match. Looks like i've figured this one out.
