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

(define (last-bar data)
  (last (data-set-rows data)))

(define (last-date data)
  (bar-date (last-bar data)))

(define (last-price pick data)
  (pick (last-bar data)))

(define (last-distance poly price data)
  (distance-to-line (- (data-set-count data) 1) price poly))

(define (bigger-gradient? p1 p2)
  (and (same-trend? p1 p2)
       (gradient> p1 p2)))

(define (inspect-for-sell data index)
  (let ((count (data-set-count data))
        (date (last-date data)))
    (let-values (((long-trend long-err)  (min-line/range/step data 0 (- count 24) 4))
                 ((short-trend short-err) (min-line/range data (- count 24) 23)))
      (and (positive? (gradient long-trend))
           (bigger-gradient? short-trend long-trend)
           (let* ((price (last-price bar-low data))
                  (val (last-distance short-trend price data))
                  (gain (last-distance long-trend price data)))
             (and (> gain 0) (< val 0)
                  (make-position index date 'sell price (+ price 0.0003) long-trend
                                 (make-pos-info gain
                                                long-trend long-err
                                                short-trend short-err))))))))

(define (inspect-for-buy data index)
  (let ((count (data-set-count data))
        (date (last-date data)))
    (let-values (((long-trend long-err)  (max-line/range/step data 0 (- count 24) 4))
                 ((short-trend short-err) (max-line/range data (- count 24) 23)))
      (and (negative? (gradient long-trend))
           (bigger-gradient? short-trend long-trend)
           (let* ((price (last-price bar-high data))
                  (val (last-distance short-trend price data))
                  (gain (- (last-distance long-trend price data))))
             (and (> gain 0) (> val 0)
                  (make-position index date 'buy price long-trend (- price 0.0003)
                                 (make-pos-info gain
                                                long-trend long-err
                                                short-trend short-err))))))))

(define *data-count* (* 24 19))

(define (close-position pos-idx price result gain)
  (list 'close pos-idx price result gain))

(define (update-position positions bar index)
  (define (adjusted-index pos)
    (+ *data-count* (- index (position-index pos))))
  (let loop ((src positions) (dest ()))
    (if (null? src)
        dest
        (let ((pos (car src)))
          (if (eq? (position-action pos) 'sell)
              (if (> (bar-low bar) (position-upper-limit pos))
                  (begin
                    (print (close-position (position-index pos) (position-upper-limit pos)
                                           'loss (- (position-price pos) (position-upper-limit pos))))
                    (loop (cdr src) dest))
                  (if (and (< (distance-to-line (adjusted-index pos) (bar-low bar)
                                                (position-lower-limit pos)) 0)
                           (> (position-price pos) (bar-high bar)))
                      (begin
                        (print (close-position (position-index pos) (bar-high bar)
                                               'gain (- (position-price pos) (bar-high bar))))
                        (loop (cdr src) dest))
                      (loop (cdr src) (cons pos dest))))

                                        ; buy
              (if (< (bar-high bar) (position-lower-limit pos))
                  (begin
                    (print (close-position (position-index pos) (position-lower-limit pos)
                                           'loss (- (position-lower-limit pos) (position-price pos))))
                    (loop (cdr src) dest))
                  (if (and (> (distance-to-line (adjusted-index pos) (bar-high bar)
                                                (position-upper-limit pos)) 0)
                           (> (bar-low bar) (position-price pos)))
                      (begin
                        (print (close-position (position-index pos) (bar-low bar)
                                               'gain (- (bar-low bar) (position-price pos))))
                        (loop (cdr src) dest))
                      (loop (cdr src) (cons pos dest)))))))))

(define (inspect date positions index)
  (let* ((data (query-data *conn* date *data-count* "1 hour"))
         (actual-date (last-date data)))
    (if (> (time-second (time-difference (date->time-utc actual-date) (date->time-utc date)))
           (* 15 60))
        (values #f positions)
        (let ((bar (last-bar data)))
          (let ((new-positions (update-position positions bar index)))
            (values (or (inspect-for-sell data index) (inspect-for-buy data index)) new-positions))))))

(define one-hour (make-time time-duration 0 (* 60 60)))

;; Function: make-date nanosecond second minute hour day month year zone-offset

(define (main . args)
  (let* ((d1 (make-date 0 0 15 0 1 4 2018 0))
         (t1 (date->time-utc d1))
         (d2 (make-date 0 0 15 0 1 4 2019 0))
         #;(d2 (make-date 0 0 15 0 5 4 2018 0))
         (t2 (date->time-utc d2)))
    (let loop ((index 0)
               (t t1)
               (positions ()))
      (if (time<? t t2)
          (let ((date (time-utc->date t)))
            (let-values (((pos poss) (inspect date positions index)))
              (when pos
                (print (date->string (position-date pos) "http://localhost:2222/~Y/~m/~d/~H/~M"))
                (print (position->string pos)))
              (if pos
                  (loop (+ index 1) (add-duration t one-hour) (cons pos poss))
                  (loop (+ index 1) (add-duration t one-hour) poss))
              ))
          (print (map position-index positions))))))
