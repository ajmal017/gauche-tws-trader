(add-load-path "./lib/")
(use scanner)
(use config)
(use redis)
(use srfi-19)

(use trader)

(define *conn* (redis-open redis-host 6379))

(define one-hour (make-time time-duration 0 (* 60 60)))

;; Function: make-date nanosecond second minute hour day month year zone-offset

(define (main . args)
  (let* ((d1 (make-date 0 0 15 0 1 10 2018 0))
         (t1 (date->time-utc d1))
         (d2 (make-date 0 0 15 0 1 11 2018 0))
         #;(d2 (make-date 0 0 15 0 5 4 2018 0))
         (t2 (date->time-utc d2)))
    (let loop ((index 0)
               (t t1)
               (positions ()))
      (if (time<? t t2)
          (let ((date (time-utc->date t)))
            (let-values (((pos poss) (inspect *conn* date positions index)))
              (when pos
                (print (date->string (position-date pos) "http://localhost:2222/~Y/~m/~d/~H/~M"))
                (print (position->string pos)))
              (if pos
                  (loop (+ index 1) (add-duration t one-hour) (cons pos poss))
                  (loop (+ index 1) (add-duration t one-hour) poss))
              ))
          (print (map position-index positions))))))
