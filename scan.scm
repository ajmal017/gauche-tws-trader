(add-load-path "./lib/")
(use scanner)
(use config)
(use redis)
(use srfi-19)

(use trader)

(define *conn* (redis-open redis-host 6379))

(define one-hour (make-time time-duration 0 (* 60 60)))
(define fifteen-mins (make-time time-duration 0 (* 60 15)))

;; Function: make-date nanosecond second minute hour day month year zone-offset

(define *eur-gbp* (make-currency-pair "EUR" "GBP"))

(define *eur-gbp-15min*
  (make-trading-style
   *eur-gbp*
   "IDEALPRO"
   "15 mins"
   "900 S"
   "960 S"
   "3 M"
   "1 W"
   ))

(define *eur-gbp-1hour*
  (make-trading-style
   *eur-gbp*
   "IDEALPRO"
   "1 hour"
   "3600 S"
   "3660 S"
   "1 Y"
   "4 W"
   ))

(define *total-gain* 0)
(define (close-positiion pos)
  (let ((gain (cadr (cdddr pos))))
    (set! *total-gain* (+ *total-gain* gain))
    (print `(close ,*total-gain* ,pos))))

(define (main . args)
  (let* ((d1 (make-date 0 0 15 0 2 7 2019 0))
         ;(d1 (make-date 0 0 0 20 23 8 2019 0))
         (t1 (date->time-utc d1))
         ;(d2 (make-date 0 0 0 2 24 8 2019 0))
         (d2 (make-date 0 0 15 0 20 9 2019 0))
         (t2 (date->time-utc d2)))
    (let loop ((index 0)
               (t t1)
               (positions ()))
      (if (time<? t t2)
          (let ((date (time-utc->date t)))
            (let-values (((pos poss)
                          (inspect *conn* *eur-gbp-15min* date positions index close-positiion)))
              (if (and pos
                       (> (pos-info-gain (position-info pos)) 0.0001)) ; > 1 pips
                  (begin
                    (write (serialize-position pos))(newline)
                    (loop (+ index 1) (add-duration t fifteen-mins) (cons pos poss)))
                  (loop (+ index 1) (add-duration t fifteen-mins) poss))
              ))
          (print (cons 'positions (map position-index positions)))))
    (print `(total ,*total-gain*))))
