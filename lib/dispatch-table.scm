(load "lib/safe-load.scm")
(load "lib/functools.scm")
(load "lib/bstree.scm")

(define (create-dispatch-record s op)
  (cons (symbol->string s) (cons s op)))

(define (dispatch-record-key r) (car r))
(define (dispatch-record-symbols r) (cadr r))
(define (dispatch-record-op r) (cddr r))

(define (dispatch-record>? r1 r2)
  (string>? (dispatch-record-key r1) (dispatch-record-key r2)))

(define (create-dispatch-table)
  (bstree-make-empty dispatch-record>?))

(define *dispatch-table* (create-dispatch-table))

(define (dispatch-tree-lookup s tree)
  (bstree-lookup (create-dispatch-record s '()) dispatch-record-key tree))

(define (dispatch-table-lookup s)
  (dispatch-tree-lookup s *dispatch-table*))

(define (dispatch-tree-insert s op tree)
  (bstree-insert tree (create-dispatch-record s op)))

(define (dispatch-table-insert s op)
  (dispatch-tree-insert s op *dispatch-table*))

(define (dispatch-tree-update s op tree)
  (bstree-update tree (create-dispatch-record s op) dispatch-record-key))

(define (dispatch-table-update s op)
  (dispatch-tree-update s op *dispatch-table*))

(define (put . l)

  (define (init op . args)
    (iter op (flatten (reverse args)) *dispatch-table*))

  (define (iter op args tree)
    (let* ((h (car args))
           (t (cdr args))
           (target (dispatch-tree-lookup h tree)))
      (if (null? t)
        (if target
          (dispatch-tree-update h op tree)
          (dispatch-tree-insert h op tree))
        (let ((sub-tree (if target
                         (dispatch-record-op target)
                         (create-dispatch-table))))
          (if target
            (dispatch-tree-update h (iter op t sub-tree) tree)
            (dispatch-tree-insert h (iter op t sub-tree) tree))))))

  (set! *dispatch-table* (apply init (reverse l)))
  'ok)

(define (get . l)
  (define (iter args tree)
    (let ((record (dispatch-tree-lookup (car args) tree)))
      (if record
        (let ((value (dispatch-record-op record)))
          (if (null? (cdr args))
            value
            (iter (cdr args) value)))
        #f)))

  (if (null? l)
    #f
    (iter (flatten l) *dispatch-table*)))

(define (display-dispatch-table)
  (define (print-node tree indent)
    (if (not (bstree-empty? tree))
      (begin
        (print-node (bstree-left-branch tree) indent)
        (let* ((record (bstree-root tree))
               (key (dispatch-record-key record))
               (val (dispatch-record-op record)))
          (display indent)
          (display "Key: ")
          (display key)
          (if (procedure? val)
            (begin
              (display " -> <procedure>")
              (newline))
            (begin
              (display " -> [Nested Table]")
              (newline)
              (print-node val (string-append indent "    ")))))
        (print-node (bstree-right-branch tree) indent))))

  (display "--- Dispatch Table ---")
  (newline)
  (print-node *dispatch-table* "")
  (display "----------------------")
  (newline))
