;;; This is gonna deviate from the book a bit in implementation, based on all i learned about
;;; tree implementations while optimising the `bstree` implementation from the last exercises
;;; so much
(load "lib/functools.scm")
(load "lib/sort.scm")

(define tag-enctree 'enctree)
(define tag-enctree-leaf 'enctree-leaf)

(define (enctree-tag tree) (car tree))
(define (enctree-tree tree) (cdr tree))
(define (enctree-root tree) (car (enctree-tree tree)))
(define (enctree-root-val tree) (car (enctree-root tree)))
(define (enctree-root-weight tree) (cdr (enctree-root tree)))
(define (enctree-branches tree) (cdr (enctree-tree tree)))
(define (enctree-left-branch tree) (car (enctree-branches tree)))
(define (enctree-right-branch tree) (cdr (enctree-branches tree)))

(define (enctree? tree) (eq? (enctree-tag tree) tag-enctree))
(define (enctree-leaf? tree) (eq? (enctree-tag tree) tag-enctree-leaf))
(define (enctree-node? tree) (or (enctree? tree) (enctree-leaf? tree)))

(define (make-enctree-leaf val weight)
  (cons tag-enctree-leaf
    (cons (cons val weight) '())))

(define (make-enctree left right)
  (let ((left-val (enctree-root-val left))
        (left-weight (enctree-root-weight left))
        (right-val (enctree-root-val right))
        (right-weight (enctree-root-weight right)))
    (cons
      tag-enctree
      (cons
        (cons
          (cond
            ((and (enctree-leaf? left) (enctree-leaf? right)) (list left-val right-val))
            ((enctree-leaf? left) (cons left-val right-val))
            ((enctree-leaf? right) (cons right-val left-val))
            (else (append left-val right-val)))
          (+ left-weight right-weight))
        (cons left right)))))

(define (letter-freq msg)
  (sort (frequencies (string->list msg) char<=? char=?) (lambda (x y) (<= (cdr x) (cdr y)))))

(define (insert-enctree tree tree-list)
  (cond
    ((null? tree-list) (list tree))
    ((<= (enctree-root-weight tree) (enctree-root-weight (car tree-list)))
      (cons tree tree-list))
    (else (cons (car tree-list) (insert-enctree tree (cdr tree-list))))))

(define (build-enctree tree-list)
  (cond
    ((null? tree-list) '())
    ((null? (cdr tree-list)) (car tree-list))
    (else
      (let* ((left (car tree-list))
             (right (cadr tree-list))
             (merged (make-enctree left right))
             (rest (cddr tree-list)))
        (build-enctree (insert-enctree merged rest))))))

(define (string->enctree msg)
  (if (string=? msg "")
    '()
    (let* ((freqs (letter-freq msg))
           (leaves (map (lambda (pair) (make-enctree-leaf (car pair) (cdr pair))) freqs)))
      (build-enctree leaves))))

(define (char-in-tree? char tree)
  (let ((symbols (enctree-root-val tree)))
    (if (enctree-leaf? tree)
      (char=? char symbols)
      (and (memv char symbols) #t))))

(define (encode-char char tree)
  (if (enctree-leaf? tree)
    '()
    (let ((left (enctree-left-branch tree))
          (right (enctree-right-branch tree)))
      (cond
        ((char-in-tree? char left) (cons 0 (encode-char char left)))
        ((char-in-tree? char right) (cons 1 (encode-char char right)))
        (else (error "Character not found in tree:" char))))))

(define (encode-list char-list tree)
  (if (null? char-list)
    '()
    (append (encode-char (car char-list) tree)
      (encode-list (cdr char-list) tree))))

(define (encode-string msg tree)
  (apply string-append (map number->string (encode-list (string->list msg) tree))))

(define (char->digit c)
  (- (char->integer c) (char->integer #\0)))

(define (string->binlist s)
  (map char->digit (string->list s)))

(define (decode-string s tree)
  (define (iter st subtree acc)
    (cond
      ((enctree-leaf? subtree)
        (let ((new-acc (cons (enctree-root-val subtree) acc)))
          (if (null? st) new-acc (iter st tree new-acc))))
      ((null? st) acc)
      (else
        (let ((sth (car st))
              (stt (cdr st)))
          (if (= sth 0)
            (iter stt (enctree-left-branch subtree) acc)
            (iter stt (enctree-right-branch subtree) acc))))))

  (list->string (reverse (iter (string->binlist s) tree '()))))

(define test-msg
  "In the beginning God created the heavens and the earth. 2 Now the earth was formless and empty, darkness was over the surface of the deep, and the Spirit of God was hovering over the waters.
3 And God said, “Let there be light,” and there was light. 4 God saw that the light was good, and he separated the light from the darkness. 5 God called the light “day,” and the darkness he called “night.” And there was evening, and there was morning—the first day.
6 And God said, “Let there be a vault between the waters to separate water from water.” 7 So God made the vault and separated the water under the vault from the water above it. And it was so. 8 God called the vault “sky.” And there was evening, and there was morning—the second day.
9 And God said, “Let the water under the sky be gathered to one place, and let dry ground appear.” And it was so. 10 God called the dry ground “land,” and the gathered waters he called “seas.” And God saw that it was good.
11 Then God said, “Let the land produce vegetation: seed-bearing plants and trees on the land that bear fruit with seed in it, according to their various kinds.” And it was so. 12 The land produced vegetation: plants bearing seed according to their kinds and trees bearing fruit with seed in it according to their kinds. And God saw that it was good. 13 And there was evening, and there was morning—the third day.
14 And God said, “Let there be lights in the vault of the sky to separate the day from the night, and let them serve as signs to mark sacred times, and days and years, 15 and let them be lights in the vault of the sky to give light on the earth.” And it was so. 16 God made two great lights—the greater light to govern the day and the lesser light to govern the night. He also made the stars. 17 God set them in the vault of the sky to give light on the earth, 18 to govern the day and the night, and to separate light from darkness. And God saw that it was good. 19 And there was evening, and there was morning—the fourth day.
20 And God said, “Let the water teem with living creatures, and let birds fly above the earth across the vault of the sky.” 21 So God created the great creatures of the sea and every living thing with which the water teems and that moves about in it, according to their kinds, and every winged bird according to its kind. And God saw that it was good. 22 God blessed them and said, “Be fruitful and increase in number and fill the water in the seas, and let the birds increase on the earth.” 23 And there was evening, and there was morning—the fifth day.
24 And God said, “Let the land produce living creatures according to their kinds: the livestock, the creatures that move along the ground, and the wild animals, each according to its kind.” And it was so. 25 God made the wild animals according to their kinds, the livestock according to their kinds, and all the creatures that move along the ground according to their kinds. And God saw that it was good.
26 Then God said, “Let us make mankind in our image, in our likeness, so that they may rule over the fish in the sea and the birds in the sky, over the livestock and all the wild animals,[a] and over all the creatures that move along the ground.”
27 So God created mankind in his own image, in the image of God he created them; male and female he created them.
28 God blessed them and said to them, “Be fruitful and increase in number; fill the earth and subdue it. Rule over the fish in the sea and the birds in the sky and over every living creature that moves on the ground.”
29 Then God said, “I give you every seed-bearing plant on the face of the whole earth and every tree that has fruit with seed in it. They will be yours for food. 30 And to all the beasts of the earth and all the birds in the sky and all the creatures that move along the ground—everything that has the breath of life in it—I give every green plant for food.” And it was so.
31 God saw all that he had made, and it was very good. And there was evening, and there was morning—the sixth day.")

(define encoding-tree (string->enctree test-msg))
(define encoded-msg (encode-string test-msg encoding-tree))
(define decoded-msg (decode-string encoded-msg encoding-tree))

(if (string=? test-msg decoded-msg)
  (display "SUCCESS!")
  (display "FAILURE!"))
