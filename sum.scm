(use srfi-19)
(use util.match)

(add-load-path "./lib/")
(use trader)

(define *positions* (make-hash-table))

(let loop ((total-gain 0))
  (let ((line (read)))
    (if (eof-object? line)
        'done
        (match line
               (('position pos-idx date action price lim1 lim2 ('pos-info gain poly1 err1 poly2 err2))
                (hash-table-put! *positions* pos-idx
                                 (make-position pos-idx
                                                (string->date date "~Y-~m-~dT~H:~M:~SZ")
                                                action
                                                price
                                                lim1
                                                lim2
                                                (make-pos-info gain poly1 err1 poly2 err2)))
                (loop total-gain))
               (('close pos-idx price result gain)
                (let* ((pos (hash-table-get *positions* pos-idx))
                       (info (position-info pos)))
                  (print #`",(pos-info-long-trend-error info) ,(pos-info-short-trend-error info) ,(pos-info-gain info)")
                  (loop (+ total-gain gain)))
                  )
               (else (loop total-gain))
               )


        ))
  )
