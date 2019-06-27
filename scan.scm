(use srfi-19)
(use math.mt-random)

(use dbi)
(use dbd.pg)

(add-load-path "./lib/")
(use trader)

(use config)

(define *conn* (dbi-connect #`"dbi:pg:user=postgres;host=,db-host"))

(define (same-trend? p1 p2)
  (> (* (poly-a p1) (poly-b p1) (poly-a p2) (poly-b p2)) 0))

(define (gradient poly)
  (- (/ (poly-a poly) (poly-b poly))))

(define (gradient> p1 p2)
  (> (abs (gradient p1))
     (abs (gradient p2))))

(define (last-price pick data)
  (pick (last (rows-of data))))

(define (last-distance poly price data)
  (distance-to-line (- (count-of data) 1) price poly))

(define (inspect date)
  (let ((data (query-data *conn* date (* 24 19) "1 hour")))
    (let ((rows (caddr data))
          (count (cadddr data)))
      (let-values (((long-trend-min long-min-dist)  (min-line/range/step data 0 (- count 24) 4))
                   ((short-trend-min short-min-dist) (min-line/range data (- count 24) 23)))
        (if (and (positive? (gradient long-trend-min))
                 (same-trend? long-trend-min short-trend-min)
                 (gradient> short-trend-min long-trend-min))
            (let* ((price (last-price low-of data))
                   (val (last-distance short-trend-min price data))
                   (earning (last-distance long-trend-min price data)))
              (if (and (> earning 0) (< val 0))
                  (begin
                    #?=long-min-dist #?=short-min-dist
                    (print #`"SELL @,price @,earning")
                    #t)
                  #f))
            (let-values (((long-trend-max long-max-dist)  (max-line/range/step data 0 (- count 24) 4))
                         ((short-trend-max short-max-dist) (max-line/range data (- count 24) 23)))
              (if (and (negative? (gradient long-trend-max))
                       (same-trend? long-trend-max short-trend-max)
                       (gradient> short-trend-max long-trend-max))
                  (let* ((price (last-price high-of data))
                         (val (last-distance short-trend-max price data))
                         (earning (- (last-distance long-trend-max price data))))
                    (if (and (> earning 0) (> val 0))
                        (begin
                          #?=long-max-dist #?=short-max-dist
                          (print #`"BUY @,price @,earning")
                          #t)
                        #f))
                  #f
                  )))))))

(define (main . args)
  (let* ((d1 (make-date 0 0 0 1 1 4 2018 0))
         (t1 (time-second (date->time-utc d1)))
         (d2 (make-date 0 0 0 1 1 2 2019 0))
         (t2 (time-second (date->time-utc d2)))
         (diff (- t2 t1))
         (mt (make <mersenne-twister>)))
    (let loop ((n 10))
      (unless (zero? n)
        (let* ((rand (mt-random-integer mt diff))
               (time (make-time time-utc 0 (+ t1 rand)))
               (date (time-utc->date time)))
          (if (inspect date)
              (begin
                (print (date->string date "http://localhost:2222/~Y/~m/~d/~H/00"))
                (loop (- n 1)))
              (loop n)
              ))))))
