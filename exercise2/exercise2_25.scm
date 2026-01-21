(define l1 '(1 3 (5 7) 9))
(define l2 '((7)))
(define l3 '(1 (2 (3 (4 (5 (6 7)))))))

(define n1 (car (cdaddr l1))) ; kind of annoying that this annotation caps out at a depth of 4, but i guess it can't be infinite without a lot of overhead
(define n2 (caar l2)) ; This one's actually convenient
(define n3 (cadadr (cadadr (cadadr l3)))) ; Well... this got dumb fast...

(newline)
(display (string-append
          "n1 = "
          (number->string n1)
          ", n2 = "
          (number->string n2)
          ", n3 = "
          (number->string n3)
          "; Result: "
          (if (= n1 n2 n3) "\033[32mSUCCESS!\033[0m\n" "\033[31mFAILURE...\033[0m\n"))) ; => SUCCESS!
(newline)
