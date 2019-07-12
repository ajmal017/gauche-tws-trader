(define-module query
  (use gauche.collection)
  (use gauche.record)
  (use srfi-19)

  (use dbi)
  (use dbd.pg)

  (use trader)

  (export
   query-data
   ))

(select-module query)

(define (query-data conn end-date count size)
  (let ((query (dbi-prepare conn "SELECT DISTINCT time, open, high, low, close FROM bars WHERE time <= to_timestamp(?) and size = ? order by time desc limit ?")))
    (let* ((end-sec (time->seconds (date->time-utc end-date)))
           (result (dbi-execute query end-sec size count))
           (getter (relation-accessor result))
           (dest (fold (lambda (row part)
                         (let ((highest (data-set-highest part))
                               (lowest (data-set-lowest part))
                               (rows (data-set-rows part))
                               (count (data-set-count part))
                               (high (string->number (getter row "high")))
                               (low (string->number (getter row "low"))))
                           (make-data-set
                            (max high highest)
                            (min low lowest)
                            (cons (make-bar
                                   (string->date (getter row "time") "~Y-~m-~d ~H:~M:~S")
                                   (string->number (getter row "open"))
                                   (string->number (getter row "close"))
                                   high
                                   low)
                                  rows)
                            (+ 1 count))))
                       (make-data-set 0 9999999 () 0) result)))
      dest)))
