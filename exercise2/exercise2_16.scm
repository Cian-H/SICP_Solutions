(load "lib/intervals.scm")

;;; I DO think i can solve this, using IDs and hashing functions to create a Merkle DAG

;;; ~~~ New functions for creating IDs ~~~

;;; String Hashing (djb2 string hasing algorithm)
(define (string-hash str)
  (let ((len (string-length str))
        (mod 655360001)) ; Large prime
    (let loop ((i 0)
               (hash 5381)) ; Magic constant
      (if (= i len)
        hash
        (loop (+ i 1)
          (modulo (+ (* hash 33) (char->integer (string-ref str i))) mod))))))

(define procedure-names '())

(define (register-name proc name)
  (set! procedure-names (cons (cons proc name) procedure-names))
  proc)

(define (get-proc-name proc)
  (let ((entry (assq proc procedure-names))) ;; assq does linear search through list til `eq?`
    (if entry
      (cdr entry)
      (error "Procedure not found!"))))

(define commutative-procedures '())

(define (register-commutative proc)
  (set! commutative-procedures (cons proc commutative-procedures)))

(define (is-commutative? proc)
  (if (memq proc commutative-procedures) #t #f))

(define (derive-compound-id id1 id2 op)
  (let* ((not-commutative (not (is-commutative? op)))
         (first-id (if (or not-commutative (< id1 id2)) id1 id2))
         (second-id (if (or not-commutative (< id1 id2)) id2 id1))
         (first-id-str (number->string first-id))
         (second-id-str (number->string second-id))
         (op-id (get-proc-name op))
         (signature (string-append "(" op-id " " first-id-str " " second-id-str ")")))
    (string-hash signature)))

(define cur-id 0)

(define (new-id)
  (set! cur-id (+ cur-id 1))
  cur-id)

;;; ~~~ Next, we monkey patch the new functionality into the library ~~~

; Decorate the bounds checks
(define lower-bound-interval-unwrapped lower-bound-interval)
(define upper-bound-interval-unwrapped upper-bound-interval)
(define (lower-bound-interval i) (lower-bound-interval-unwrapped (car i)))
(define (upper-bound-interval i) (upper-bound-interval-unwrapped (car i)))

; Correct the interval bounds sorting function
(define (sort-interval x) (cons (lower-bound-interval-unwrapped x) (upper-bound-interval-unwrapped x)))

; Wrap the constructor
(define make-interval-unwrapped make-interval)

(define (attach-id i id)
  (cons i id))

(define (make-interval a b) (attach-id (make-interval-unwrapped a b) (new-id)))

; Next, we register and wrap the core arithmetic operations we need
(define add-interval-unwrapped add-interval)
(define sub-interval-unwrapped sub-interval)
(define mul-interval-unwrapped mul-interval)
(define div-interval-unwrapped div-interval)

(register-name add-interval-unwrapped "+")
(register-name sub-interval-unwrapped "-")
(register-name mul-interval-unwrapped "*")
(register-name div-interval-unwrapped "/")

(register-commutative add-interval-unwrapped)
(register-commutative mul-interval-unwrapped)

(define (op-wrapper op)
  (lambda (a b)
    (let ((a-id (cdr a))
          (b-id (cdr b)))
      (attach-id
        (car (op a b))
        (derive-compound-id a-id b-id op)))))

(define add-interval (op-wrapper add-interval-unwrapped))
(define sub-interval (op-wrapper sub-interval-unwrapped))
(define mul-interval (op-wrapper mul-interval-unwrapped))

(define (div-interval a b)
  (let* ((a-id (cdr a))
         (b-id (cdr b))
         (new-id (derive-compound-id a-id b-id div-interval-unwrapped)))
    (attach-id
      (if (= a-id b-id)
        (make-interval-unwrapped 1 1)
        (car (div-interval-unwrapped a b)))
      new-id)))

;;; ~~~ Finally, we test! ~~~

(define A (make-interval 1024 1042))
(define B (make-interval 10004 10005))

(let* ((AdA (div-interval A A))
       (BdB (div-interval B B))
       (AdB (div-interval A B))
       (pAdA (* 100 (percent-interval AdA)))
       (pBdB (* 100 (percent-interval BdB)))
       (pAdB (* 100 (percent-interval AdB)))
       (AdAs (interval->string AdA))
       (BdBs (interval->string BdB))
       (AdBs (interval->string AdB))
       (pAdAs (number->string (exact->inexact pAdA)))
       (pBdBs (number->string (exact->inexact pBdB)))
       (pAdBs (number->string (exact->inexact pAdB))))
  (display (string-append
            (string-append "A + B = " (interval->string (add-interval A B)) "\n")
            (string-append "A - B = " (interval->string (sub-interval A B)) "\n")
            (string-append "A * B = " (interval->string (mul-interval A B)) "\n")
            (string-append "A / A = " AdAs " (" pAdAs "%)\n")
            (string-append "B / B = " BdBs " (" pBdBs "%)\n")
            (string-append "A / B = " AdBs " (" pAdBs "%)\n")
            "Result: "
            (if (and (is-close? (+ pAdA pBdB) 0.0) (not (is-close? pAdB 0.0)))
              "\033[32mSUCCESS!\033[0m\n"
              "\033[31mFAILURE...\033[0m\n"))))

;;; Result: Success! Everything is looking correct, and the identity divs return [1, 1]

;;; I DO notice one issue though. If i run the following i see:

; > (add-interval A (add-interval A B))
; (mcons (mcons 12052 12089) 103585437)
; > (add-interval B (add-interval A A))
; (mcons (mcons 12052 12089) 145044469)

;;; These *should* have the same id because they are a commutative combination of the same 3
;;; intervals. However, they clearly do NOT have the same id. It seems this only solves it for the
;;; case of direct operations; as soon as we combine them it breaks down.
