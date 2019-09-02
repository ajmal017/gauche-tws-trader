(define-module query
  (use gauche.collection)
  (use gauche.record)
  (use srfi-19)
  (use util.match)

  (use redis)
  (use redis.async)

  (use trader)

  (export
   query-data
   add-data
   ))

(select-module query)


;; Redis
;; ZSET symbol . bar-size [ time ] -> (time open close high low)
(define (make-redis-key symbol bar-size) #`",|symbol|:,|bar-size|")

(define (query-data conn symbol end-date count size)
  (let* ((end-sec (time->seconds (date->time-utc end-date)))
         (result (redis-zrevrangebyscore conn (make-redis-key symbol size)
                                         end-sec 0 "limit" 0 count))
         (dest (fold (lambda (row part)
                       (match (read-from-string row)
                              [(time open close high low)
                               (let ((highest (data-set-highest part))
                                     (lowest (data-set-lowest part))
                                     (rows (data-set-rows part))
                                     (count (data-set-count part)))
                                 (make-data-set
                                  (max high highest)
                                  (min low lowest)
                                  (cons (make-bar
                                         (time-utc->date (make-time time-utc 0 time))
                                         open
                                         close
                                         high
                                         low)
                                        rows)
                                  (+ 1 count)))])
                       )
                     (make-data-set 0 9999999 () 0)
                     result)))
    dest))

(define (add-data conn symbol bar-size date open close high low)
  (let ((time (time-second (date->time-utc date))))
    (let ((result (redis-zadd conn (make-redis-key symbol bar-size)
                              time (write-to-string `(,time ,open ,close ,high ,low)))))
      (unless (= result 1)
        (display #`"Failed to add data. Result: ,result" (current-error-port))
        (newline (current-error-port))))))
