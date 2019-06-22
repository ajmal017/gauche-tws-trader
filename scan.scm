(use srfi-19)
(use math.mt-random)

(use dbi)
(use dbd.pg)

(add-load-path "./lib/")
(use trader)

(use config)

(define *conn* (dbi-connect #`"dbi:pg:user=postgres;host=,db-host"))

(define poly-a car)
(define poly-b cadr)
(define poly-c caddr)

(define (inspect date)
  (let ((data (query-data *conn* date (* 24 19) "1 hour")))
    (let ((highest (car data))
          (lowest (cadr data))
          (rows (caddr data))
          (count (cadddr data)))
      (let ((long-trend-min  (min-line/range/step data 0 (- count 24) 4))
            (short-trend-min (min-line/range data (- count 24) 23))
            (long-trend-max  (max-line/range/step data 0 (- count 24) 4))
            (short-trend-max (max-line/range data (- count 24) 23)))
        (if (and (negative? (poly-b long-trend-min))
                 (> (* (poly-b long-trend-min) (poly-b short-trend-min)) 0)
                 (> (poly-b long-trend-min) (poly-b short-trend-min)))
            (let* ((last-low (low-of (last rows)))
                   (b (poly-b short-trend-min))
                   (c (poly-c short-trend-min))
                   (val (+ (- count 1) (* b last-low) c)))
              (if (< val 0)
                  (print "SELL!")
                  (print "ready to sell"))
              #t)
            (if (and (positive? (poly-b long-trend-max))
                     (> (* (poly-b long-trend-max) (poly-b short-trend-max)) 0)
                     (< (poly-b long-trend-min) (poly-b short-trend-min)))
                (let* ((last-high (high-of (last rows)))
                       (b (poly-b short-trend-max))
                       (c (poly-c short-trend-max))
                       (val (+ (- count 1) (* b last-high) c)))
                  (if (> val 0)
                      (print "BUY!")
                      (print "ready to buy"))
                  #t)
                #f
                ))))))

(define (main . args)
  (let* ((d1 (make-date 0 0 0 1 1 4 2018 0))
         (t1 (time-second (date->time-utc d1)))
         (d2 (make-date 0 0 0 1 1 2 2019 0))
         (t2 (time-second (date->time-utc d2)))
         (diff (- t2 t1))
         (mt (make <mersenne-twister>)))
    (let loop ((n 1000))
      (unless (zero? n)
        (let* ((rand (mt-random-integer mt diff))
               (time (make-time time-utc 0 (+ t1 rand)))
               (date (time-utc->date time)))
          (when (inspect date)
            (let ((date+1 (time-utc->date (add-duration time (make-time time-duration 0 (* 24 60 60))))))
              (print (date->string date+1 "http://localhost:2222/~Y/~m/~d/~H/00"))))
          (loop (- n 1)))))))
