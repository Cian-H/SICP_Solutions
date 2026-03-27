(load "lib/safe-load.scm")
(load "lib/functools.scm")
(load "lib/typing.scm")
(load "lib/dispatch-table.scm")
(load "lib/generic-dispatch.scm")

;; Retrieves the value associated with ATTR from OBJ.
;; Extracts the underlying alist data from the typed object.
;; Returns the value if found, or #f if the attribute does not exist.
(define (get-attribute obj attr)
  (let* ((data (type-unwrap obj))
         (pair (assoc attr data)))
    (if pair
      (cdr pair)
      #f)))

;; Non-destructively creates a new object by prepending a new attribute
;; ATTRNAME with value VAL to the data of OBJ.
;; Returns the newly created wrapped object.
(define (add-attribute obj attrname val)
  (let* ((type (type-of obj))
         (data (type-unwrap obj))
         (new-data (cons (cons attrname val) data)))
    (type-wrap type new-data)))

;; Destructively updates the value of ATTR in OBJ to VAL.
;; If the attribute exists, its value is mutated. If it does not exist,
;; it creates a new attribute and mutates the object's cdr in-place.
;; Returns the modified object.
(define (set-attribute obj attr val)
  (let* ((data (type-unwrap obj))
         (pair (assoc attr data)))
    (if pair
      (begin
        (set-cdr! pair val)
        obj)
      (let ((new-obj (add-attribute obj attr val)))
        (set-cdr! obj (type-unwrap new-obj))
        obj))))

;; Constructs and returns an object of TYPE with the given ATTRLIST.
;; Automatically injects a self-referential 'self' attribute pointing
;; back to the instantiated object.
(define (make-object type . attrlist)
  (let* ((self-referential-attrlist (cons (cons 'self #f) attrlist))
         (self (type-wrap type self-referential-attrlist)))
    (set-attribute self 'self self)
    self))

;; Syntax: (object type (key value) ...)
;; Macro providing syntactic sugar to instantiate an object of TYPE
;; with a sequence of key-value pairs representing attributes.
(define-syntax object
  (syntax-rules ()
    ((_ type (key value) ...)
      (make-object 'type (cons 'key value) ...))))

;; Registers a procedure PROC as a method named METHOD-NAME
;; for the specified TYPE in the global dispatch table.
(define (define-method type method-name proc)
  (put method-name type proc))

;; Establishes an inheritance relationship where CHILD-TYPE inherits
;; from PARENT-TYPE. This is stored as a coercion edge in the dispatch table.
(define (define-parent child-type parent-type)
  (put 'coerce child-type parent-type (lambda (x) x)))

;; Invokes METHOD-NAME on OBJ with the provided ARGS.
;; Performs a breadth-first search (BFS) through the object's type
;; and its ancestor graph to resolve the method dynamically.
;; Throws an error if the method cannot be found in the inheritance tree.
(define (invoke obj method-name . args)
  (let* ((start-type (type-of obj)))
    (define (search-method queue visited)
      (if (null? queue)
        #f
        (let* ((current-type (car queue))
               (method (get method-name current-type)))
          (if method
            method
            (let ((edges-bst (get 'coerce current-type)))
              (if (not edges-bst)
                (search-method (cdr queue) visited)
                (let* ((records (bstree->list edges-bst))
                       (parent-types (map dispatch-record-symbols records))
                       (unvisited (filter (lambda (n) (not (memq n visited))) parent-types)))
                  (search-method (append (cdr queue) unvisited)
                    (append unvisited visited)))))))))

    (let ((method (search-method (list start-type) (list start-type))))
      (if method
        (apply method (cons obj args))
        (error "Method not found in object or ancestors: " method-name start-type)))))

;; Syntax: (define-class (class-name arg ...)
;;           (attributes (attr-name attr-val) ...)
;;           (methods (method-name (self-arg m-arg ...) body ...) ...)
;;           init-body ...)
;; Macro to define a new class. It generates a constructor procedure,
;; sets up default attributes, registers its methods in the dispatch table,
;; and executes any provided initialization body.
(define-syntax define-class
  (syntax-rules (attributes methods)
    ((_ (class-name arg ...)
        (attributes (attr-name attr-val) ...)
        (methods (method-name (self-arg m-arg ...) body ...) ...)
        init-body
        ...)
      (begin
        (define (class-name arg ...)
          (let* ((self (make-object 'class-name (cons 'attr-name attr-val) ...)))
            init-body
            ...
            self))

        (define-method 'class-name 'method-name
          (lambda (self-arg m-arg ...)
            body
            ...))
        ...))))

;; Syntax: (@ obj method-name arg ...)
;; Syntactic sugar for method invocation. Equivalent to calling `invoke`.
(define-syntax @
  (syntax-rules ()
    ((_ obj method-name arg ...)
      (invoke obj 'method-name arg ...))))

;; Syntax: ($ obj attr-name)      --> Getter
;;         ($ obj attr-name val)  --> Setter
;; Syntactic sugar for retrieving or mutating an object's attributes.
(define-syntax $
  (syntax-rules ()
    ((_ obj attr-name)
      (get-attribute obj 'attr-name))
    ((_ obj attr-name val)
      (set-attribute obj 'attr-name val))))
