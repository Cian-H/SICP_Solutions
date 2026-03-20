(load "lib/safe-load.scm")
(load "lib/dispatch-table.scm")

(define (put-coercion t1 t2 proc)
  (put 'coerce t1 t2 proc))

(define (get-coercion t1 t2)
  (get 'coerce t1 t2))

(define (find-coercion-path start-type target-type)
  (define (bfs queue visited)
    (if (null? queue)
      #f ; No path found
      (let* ((path (car queue))
             (current-type (car path)))
        (if (eq? current-type target-type)
          (reverse path) ; Path found!
          (let ((edges-bst (get 'coerce current-type)))
            ;; If the type has no outgoing coercions, get returns #f
            (if (not edges-bst)
              (bfs (cdr queue) visited)
              ;; Otherwise, extract the connected types from the BST
              (let* ((records (bstree->list edges-bst))
                     (neighbors (map dispatch-record-symbols records))
                     (unvisited (filter (lambda (n) (not (memq n visited))) neighbors))
                     (new-paths (map (lambda (n) (cons n path)) unvisited)))
                (bfs (append (cdr queue) new-paths)
                  (append unvisited visited)))))))))
  ;; Start the queue with a path containing only the start-type
  (bfs (list (list start-type)) (list start-type)))

(define (apply-coercion-path val path)
  (if (null? (cdr path))
    val
    (let* ((from-type (car path))
           (to-type (cadr path))
           (coerce-proc (get-coercion from-type to-type)))
      (apply-coercion-path (coerce-proc val) (cdr path)))))

(define (coerce-type val target-type)
  (let* ((from-type (type-of val))
         (path (find-coercion-path from-type target-type)))
    (if path
      (apply-coercion-path val path)
      #f)))

(define (apply-generic op . args)
  (let* ((types (map type-of args))
         (proc (get op types)))
    (if proc
      ;; Exact match found
      (apply proc (map type-unwrap args))
      ;; No exact match, try type coercions
      (if (= (length args) 2)
        (let* ((type1 (car types))
               (type2 (cadr types))
               (a1 (car args))
               (a2 (cadr args)))
          (if (eq? type1 type2)
            (error "apply-generic: no method for types" (list op types))
            (let ((coerced-a1 (coerce-type a1 type2)))
              (if coerced-a1
                (apply-generic op coerced-a1 a2)
                (let ((coerced-a2 (coerce-type a2 type1)))
                  (if coerced-a2
                    (apply-generic op a1 coerced-a2)
                    (error "apply-generic: no valid coercion graph path for types" (list op types))))))))
        (error "apply-generic: no method for types" (list op types))))))
