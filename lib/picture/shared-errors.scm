(define (_monotype-error s t)
  (error (string-append s " only supports `" t "` arguments")))
