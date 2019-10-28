(use util.match)
(use srfi-19)

(use redis)

(add-load-path "./lib/")
(use scanner)
(use config)
(use trader)

(define *conn* (redis-open redis-host 6379))

(define one-hour (make-time time-duration 0 (* 60 60)))
(define fifteen-mins (make-time time-duration 0 (* 60 15)))

;; Function: make-date nanosecond second minute hour day month year zone-offset

(define (make-style sym cur)
  (make-trading-style
   (make-currency-pair sym cur)
   "IDEALPRO"
   "15 mins"
   "900 S"
   "960 S"
   "3 M"
   "1 W"
   ))

(define *total-gain* 0)
(define *total-orders* 0)

(define (close-positiion index)
  (lambda (pos)
    (let ((gain (cadr (cdddr pos))))
      (set! *total-gain* (+ *total-gain* gain))
      (inc! *total-orders*)
      (print `(close ,*total-orders* ,*total-gain* ,index ,pos)))))

(define (main args)
  (match args ((com sym cur) (scan-main sym cur))))

(define (scan-main sym cur)
  (print (list 'scan sym cur))
  (let* ((d1 (make-date 0 0 0 0 18 9 2019 0))
         ;(d1 (make-date 0 0 0 20 23 8 2019 0))
         (t1 (date->time-utc d1))
         ;(d2 (make-date 0 0 0 2 24 8 2019 0))
         (d2 (make-date 0 0 0 0 18 10 2019 0))
         (t2 (date->time-utc d2))
         (style (make-style sym cur)))
    (let loop ((index 0)
               (t t1)
               (positions ()))
      (if (time<? t t2)
          (let ((date (time-utc->date t)))
            (let-values (((pos poss)
                          (inspect *conn* style date positions index (close-positiion index))))
              (if (and pos
                       (> (pos-info-gain (position-info pos)) 0.0010)) ; > 10 pips
                  (begin
                    (inc! *total-orders*)
                    (write (serialize-position pos))(newline)
                    (loop (+ index 1) (add-duration t fifteen-mins) (cons pos poss)))
                  (loop (+ index 1) (add-duration t fifteen-mins) poss))
              ))
          (print (cons 'positions (map position-index positions)))))
    (print `(total ,*total-orders* ,*total-gain*)))
  0)
