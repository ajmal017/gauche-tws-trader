(use util.match)
(use srfi-19)

(use redis)

(add-load-path "./lib/")
(use scanner)
(use config)
(use trader)

(define *conn* (redis-open redis-host redis-port))

(define keys (list
              "EUR.CHF:15 mins"
              "EUR.GBP:15 mins"
              "EUR.USD:15 mins"
              "GBP.CHF:15 mins"
              "GBP.USD:15 mins"
              "USD.CHF:15 mins"))

(define (main . args)
  (for-each (lambda (key) (clean-up key)) keys))

(define dup-count 0)

(define (clean-up key)
  (print #`"Processing ,key")
  (let ((count (redis-zcard *conn* key)))
    (let loop ((i 0) (j 0) (entries #f) (prev-score ""))
      (when (< i count)
            (if (zero? j)
                (let ((entries (redis-zrange *conn* key i (+ i 1000) "withscores")))
                  (unless (zero? (vector-length entries))
                          (loop i (/ (vector-length entries) 2) entries prev-score)))
                (let* ((len (/ (vector-length entries) 2))
                       (k (* 2 (- len j)))
                       (dat (vector-ref entries k))
                       (score (vector-ref entries (+ 1 k))))
                  (when (string=? score prev-score)
                        (let ((records (redis-zrangebyscore *conn* key score score)))
                          (unless (> (vector-length records) 1)
                                  (error #`"something went wrong ,records"))
                          (let ((num (redis-zrem *conn* key dat)))
                            (unless (string=? (number->string num) "1")
                                    (error #`"something went wrong ,records zrem result: ,num")))
                          (inc! dup-count)))
                  (loop (+ i 1) (- j 1) entries score))))))
  (print #`"dup-count ,dup-count"))
