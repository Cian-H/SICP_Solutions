(load "lib/huffman.scm")

(define msg "Get a job\nSha na na na na na na na na\nGet a job\nSha na na na na na na na na\nWah yip yip yip yip yip yip yip yip yip\nSha boom")

; Lets start with the second half of the question, which is easy to answer:
;;; What is the smallest number of bits that would be needed to encode this song if we used a
;;; fixed-length code for the eight-symbol alphabet?
; This string has 17 unique values (including newlines and spaces), so we would need at least
; `(ceiling (log 17 2)) ; => 5` bits. The string has 124 characters, so this string would require
; 620 bits with a fixed length code.

; Moving back to the first part of the question: how many bits would this take with Huffman
; encoding? To identify this, we need to know how many of each character we have. A quick count
; finds we have: `W:1,m:1,G:2,e:2,j:2,t:2,S:3,b:3,h:4,o:4,\n:5,i:9,p:9,y:9,n:16,a:22, :30` sorted
; by weight. If we manually perform the merges we see the following structure emerge:

; W:1,m:1,G:2,e:2,j:2,t:2,S:3,b:3,h:4,o:4,\n:5,i:9,p:9,y:9,n:16,a:22, :30
; G:2,e:2,j:2,t:2,Wm:2,S:3,b:3,h:4,o:4,\n:5,i:9,p:9,y:9,n:16,a:22, :30
; j:2,t:2,Wm:2,S:3,b:3,h:4,o:4,Ge:4,\n:5,i:9,p:9,y:9,n:16,a:22, :30
; Wm:2,S:3,b:3,h:4,o:4,Ge:4,jt:4,\n:5,i:9,p:9,y:9,n:16,a:22, :30
; b:3,h:4,o:4,Ge:4,jt:4,\n:5,WmS:5,i:9,p:9,y:9,n:16,a:22, :30
; o:4,Ge:4,jt:4,\n:5,WmS:5,bh:7,i:9,p:9,y:9,n:16,a:22, :30
; jt:4,\n:5,WmS:5,bh:7,oGe:8,i:9,p:9,y:9,n:16,a:22, :30
; WmS:5,bh:7,oGe:8,i:9,p:9,y:9,jt\n:9,n:16,a:22, :30
; oGe:8,i:9,p:9,y:9,jt\n:9,WmSbh:12,n:16,a:22, :30
; p:9,y:9,jt\n:9,WmSbh:12,n:16,oGei:17,a:22, :30
; jt\n:9,WmSbh:12,n:16,oGei:17,py:18,a:22, :30
; n:16,oGei:17,py:18,jt\nWmSbh:21,a:22, :30
; py:18,jt\nWmSbh:21,a:22, :30,noGei:33
; a:22, :30,noGei:33,pyjt\nWmSbh:39
; noGei:33,pyjt\nWmSbh:39,a :52
; a :52,noGeipyjt\nWmSbh:72
; anoGeipyjt\nWmSbh:124

; Since each merge represents a node we must pass through, we can trace each characters through this
; structure and count the number of merges they are in to get the number of kits they need for
; their encodings. If we follow each character through the structure this way we get the following
; encoding lengths for this tree:
; W:7,m:7,G:6,e:6,j:6,t:6,S:6,b:6,h:6,o:5,\n:5,i:4,p:4,y:4,n:3,a:2, :2
; Mapping this back onto our original character counts gives us an encoded string length of:
; `(+ 7 7 (* 6 2) (* 6 2) (* 6 2) (* 6 2) (* 6 3) (* 6 3) (* 6 4) (* 5 4) (* 5 5) (* 4 9) (* 4 9)
; (* 4 9) (* 3 16) (* 2 22) (* 2 30)) ; => 427` bits. This gives us a compression ratio of
; `(/ 427 620) ; => 0.689` relative to the most efficient possible fixed length encoding for this
; string.

; To verify this calculation, we can check the length of the encoded string:
(define t (string->enctree msg))
(define encoded-msg (encode-string msg t))
(newline)
(display (string-length encoded-msg)) ; => 427
(newline)

; Confirmed!

; Oops, just realised that it actually asked for us to encode the words as symbols (i.e: tokens)
; not as letters! I don't want to redo the whole process, but now that i've proven i can do it
; above lets just do this one programatically:

(define tokenized-msg
  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(define token-tree (list->enctree tokenized-msg))
(define token-encoded (encode-list tokenized-msg token-tree))
(newline)
(display (length token-encoded)) ; => 100
(newline)
