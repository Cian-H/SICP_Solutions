(load "lib/intervals.scm")

(define A (make-interval 1024 1042))
(define B (make-interval 10004 10005))

(let* ((AA (mul-interval A A))
       (BB (mul-interval B B))
       (AB (mul-interval A B))
       (BA (mul-interval B A))
       (AdA (div-interval A A))
       (BdB (div-interval B B))
       (AdB (div-interval A B))
       (BdA (div-interval B A))
       (pAA (* 100 (percent-interval AA)))
       (pBB (* 100 (percent-interval BB)))
       (pAB (* 100 (percent-interval AB)))
       (pBA (* 100 (percent-interval BA)))
       (pAdA (* 100 (percent-interval AdA)))
       (pBdB (* 100 (percent-interval BdB)))
       (pAdB (* 100 (percent-interval AdB)))
       (pBdA (* 100 (percent-interval BdA)))
       (AAs (interval->string AA))
       (BBs (interval->string BB))
       (ABs (interval->string AB))
       (BAs (interval->string BA))
       (AdAs (interval->string AdA))
       (BdBs (interval->string BdB))
       (AdBs (interval->string AdB))
       (BdAs (interval->string BdA))
       (pAAs (number->string (exact->inexact pAA)))
       (pBBs (number->string (exact->inexact pBB)))
       (pABs (number->string (exact->inexact pAB)))
       (pBAs (number->string (exact->inexact pBA)))
       (pAdAs (number->string (exact->inexact pAdA)))
       (pBdBs (number->string (exact->inexact pBdB)))
       (pAdBs (number->string (exact->inexact pAdB)))
       (pBdAs (number->string (exact->inexact pBdA))))
  (display (string-append
            "A * A = "
            AAs
            " ("
            pAAs
            "%)\n"
            "B * B = "
            BBs
            " ("
            pBBs
            "%)\n"
            "A * B = "
            ABs
            " ("
            pABs
            "%)\n"
            "B * A = "
            BAs
            " ("
            pBAs
            "%)\n"
            "A / A = "
            AdAs
            " ("
            pAdAs
            "%)\n"
            "B / B = "
            BdBs
            " ("
            pBdBs
            "%)\n"
            "A / B = "
            AdBs
            " ("
            pAdBs
            "%)\n"
            "B / A = "
            BdAs
            " ("
            pBdAs
            "%)\n")))

;;; Found the smoking gun! The A/A and B/B cases show large percentage discrepancies:
; A * A = [1048576, 1085764] (1.7423653213639814%)
; B * B = [100080016, 100100025] (0.00999550199912288%)
; A * B = [10244096, 10425210] (0.8762461594017719%)
; B * A = [10244096, 10425210] (0.8762461594017719%)
; A / A = [0.982725527831094, 1.017578125] (1.742365321363972%)
; B / B = [0.9999000499750125, 1.0000999600159937] (0.009995501999132402%)
; A / B = [0.1023488255872064, 0.10415833666533386] (0.8762461594017685%)
; B / A = [9.600767754318618, 9.7705078125] (0.8762461594017741%)

;;; So, if we repeat a variable in any case where division occurs we suddenly get errors that can
;;; snowball out of control. This is why `par1` is better!

;;; The question is: how do we change the `div-interval` function to fix this without a significant
;;; redesign of the function?
;;; My first instinct would be to just do an equality check, but that doesn't actually work because
;;; What if the intervals being divided are 2 different intervals? The error of these 2 intervals
;;; should still compound in this case! I would need to implement some way to trace the origin
;;; of an interval to be able to fix this issue.
