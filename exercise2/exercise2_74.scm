;; We use data directed dispatch to have a different record fetcher
;; per-file assuming they have a division dependent type
(define (get-record employee file)
  (let* ((division (type-tag file))
         (raw-record ((get 'get-record division) employee (contents file))))
    (if raw-record
      (attach-tag division raw-record)
      '())))

;; Here, we use the generic record-fetcher and we dispatch a salary
;; getter from our dispatch table to parse the record. This allows
;; us to leave the record structure up to the individual divisions
;; (which, according to the question is how they've been operating
;; so far!). The only thing we can do is leave it up to the divisions
;; to write their own parsers.
(define (get-salary record)
  (let ((division (type-tag record)))
    ((get 'get-salary division) (contents record))))

;; We iterate every division looking for the employee record, and
;; return it if found
(define (find-employee-record employee file-list)
  (if (null? file-list)
    '()
    (let ((file (car file-list)))
      (let ((record (get-record employee file)))
        (if (null? record)
          (find-employee-record employee (cdr file-list))
          record)))))

;; When insatiable add a new division, their file just need to provide
;; a type tag and the associated parsers for their type. That being said:
;; maybe they should get their act together. Agree on a schema, lads!
;; Even if this is an *improvement* its still brittle as hell and relies
;; on divisions acting as good citizens in this system. Modern systems
;; design experience tells us that is a *recipe* for disaster!
