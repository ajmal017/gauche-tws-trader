(use srfi-19)
(use gauche.record)

(use math.mt-random)
(use text.tree)

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

(define (last-date data)
  (bar-date (last (data-set-rows data))))

(define (last-price pick data)
  (pick (last (data-set-rows data))))

(define (last-distance poly price data)
  (distance-to-line (- (data-set-count data) 1) price poly))

(define-record-type position #t #t
  date
  action
  price
  upper-limit
  lower-limit
  info)

(define (position->string pos)
  (tree->string
   (intersperse
    " "
    (list "Pos"
          (date->string (position-date pos))
          (position-action pos)
          (position-price pos)
          (position-upper-limit pos)
          (position-lower-limit pos)
          (pos-info->string (position-info pos))))))

(define (poly->string poly)
  #`"Poly (,(poly-a poly) ,(poly-b poly) ,(poly-c poly))")

(define (pos-info->string info)
  #`"Info ,(poly->string (pos-info-long-trend-poly info)) ,(pos-info-long-trend-error info) ,(poly->string (pos-info-short-trend-poly info)) ,(pos-info-short-trend-error info)")

(define-record-type pos-info #t #t
  long-trend-poly
  long-trend-error
  short-trend-poly
  short-trend-error)

(define-record-type result #t #t
  closed-at
  position
  gain)

(define (inspect-for-sell data)
  (let ((count (data-set-count data))
        (date (last-date data)))
    (let-values (((long-trend long-err)  (min-line/range/step data 0 (- count 24) 4))
                 ((short-trend short-err) (min-line/range data (- count 24) 23)))
      (if (and (positive? (gradient long-trend))
               (same-trend? long-trend short-trend)
               (gradient> short-trend long-trend))
          (let* ((price (last-price bar-low data))
                 (val (last-distance short-trend price data))
                 (optimal-earning (last-distance long-trend price data)))
            (if (and (> optimal-earning 0) (< val 0))
                (make-position date 'sell price (+ price 0.0003) long-trend
                               (make-pos-info long-trend long-err
                                              short-trend short-err))
                #f))
          #f))))

(define (inspect-for-buy data)
  (let ((count (data-set-count data))
        (date (last-date data)))
    (let-values (((long-trend long-err)  (max-line/range/step data 0 (- count 24) 4))
                 ((short-trend short-err) (max-line/range data (- count 24) 23)))
      (if (and (negative? (gradient long-trend))
               (same-trend? long-trend short-trend)
               (gradient> short-trend long-trend))
          (let* ((price (last-price bar-high data))
                 (val (last-distance short-trend price data))
                 (optimal-earning (- (last-distance long-trend price data))))
            (if (and (> optimal-earning 0) (> val 0))
                (make-position date 'buy price long-trend (- price 0.0003)
                               (make-pos-info long-trend long-err
                                              short-trend short-err))
                #f))
          #f))))

(define (inspect date)
  (let ((data (query-data *conn* date (* 24 19) "1 hour")))
    (or (inspect-for-sell data) (inspect-for-buy data))))

(define one-hour (make-time time-duration 0 (* 60 60)))

(define (main . args)
  (let* ((d1 (make-date 0 0 0 1 1 4 2018 0))
         (t1 (date->time-utc d1))
         (d2 (make-date 0 0 0 1 10 4 2018 0))
         (t2 (date->time-utc d2)))
    (let loop ((t t1))
      (when (time<? t t2)
        (let* ((date (time-utc->date t))
               (pos (inspect date)))
          (when pos
            (print (date->string date "http://localhost:2222/~Y/~m/~d/~H/00"))
            (print (position->string pos)))
          (loop (add-duration t one-hour))
          )))))
