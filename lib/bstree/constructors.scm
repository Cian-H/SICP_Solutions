(define (bstree . l)
  (bstree-balance (list->bstree (unique (sort-ascending l)))))

(define (list->bstree sorted-list)
  (if (null? sorted-list)
    bstree-empty
    (car (bstree-partial sorted-list (length sorted-list)))))

(define (bstree->list tree)
  (cond
    ((bstree-empty? tree) '())
    ((bstree-end? (bstree-branches tree)) (list (bstree-root tree)))
    (else
      (let ((root (bstree-root tree))
            (left (bstree-left-branch tree))
            (right (bstree-right-branch tree)))
        (append (bstree->list left)
          (cons root (bstree->list right)))))))

(define (bstree->string tree)
  (define (val->string val)
    (cond ((number? val) (number->string val))
      ((symbol? val) (symbol->string val))
      ((string? val) val)
      (else "<?>")))

  (define (build-string t prefix is-left)
    (if (bstree-empty? t)
      ""
      (let* ((right (bstree-right-branch t))
             (left (bstree-left-branch t))
             (right-str (build-string right
                         (string-append prefix (if is-left "│   " "    "))
                         #f))
             (left-str (build-string left
                        (string-append prefix (if is-left "    " "│   "))
                        #t))
             (node-str (string-append prefix
                        (if (string=? prefix "")
                          ""
                          (if is-left "└── " "┌── "))
                        (val->string (bstree-root t))
                        "\n")))
        (string-append right-str node-str left-str))))

  (if (bstree-empty? tree)
    "<empty>\n"
    (build-string tree "" #f)))
