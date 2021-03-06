(define-module scanner
  (use srfi-19)
  (use gauche.record)

  (use math.mt-random)
  (use text.tree)

  (add-load-path "./lib/")
  (use trader)
  (use query)

  (export inspect)
  )

(select-module scanner)

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
           (let* ((price (last-price bar-close data))
                  (lmt-price (last-price bar-high data))
                  (val (last-distance short-trend price data))
                  (gain (last-distance long-trend price data)))
             (and (> gain 0) (< val 0)
                  (make-position index date 'sell price (+ lmt-price 0.0001) long-trend
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
           (let* ((price (last-price bar-close data))
                  (lmt-price (last-price bar-low data))
                  (val (last-distance short-trend price data))
                  (gain (- (last-distance long-trend price data))))
             (and (> gain 0) (> val 0)
                  (make-position index date 'buy price long-trend (- lmt-price 0.0001)
                                 (make-pos-info gain
                                                long-trend long-err
                                                short-trend short-err))))))))

(define *data-count* (* 24 19))

(define (close-position pos-idx price result gain)
  (list 'close pos-idx price result gain))

(define (update-position positions bar index close-proc)
  (define (adjusted-index pos)
    (+ *data-count* (- (x->number index) (x->number (position-index pos)))))
  (let loop ((src positions) (dest ()))
    (if (null? src)
        dest
        (let* ((entry (car src))
               (pos-id (car entry))
               (pos (cdr entry)))
          (if (eq? (position-action pos) 'sell)
              (if (> (bar-close bar) (position-upper-limit pos))
                  (begin
                    (close-proc pos-id (close-position (position-index pos) (position-upper-limit pos)
                                                'loss (- (position-price pos)
                                                         (position-upper-limit pos))))
                    (loop (cdr src) dest))
                  (if (and (< (distance-to-line (adjusted-index pos) (bar-close bar)
                                                (position-lower-limit pos)) 0)
                           (> (position-price pos) (bar-close bar)))
                      (begin
                        (close-proc pos-id (close-position (position-index pos) (bar-close bar)
                                                    'gain (- (position-price pos) (bar-close bar))))
                        (loop (cdr src) dest))
                      (loop (cdr src) (cons entry dest))))

                                        ; buy
              (if (< (bar-close bar) (position-lower-limit pos))
                  (begin
                    (close-proc pos-id (close-position (position-index pos) (position-lower-limit pos)
                                                'loss (- (position-lower-limit pos)
                                                         (position-price pos))))
                    (loop (cdr src) dest))
                  (if (and (> (distance-to-line (adjusted-index pos) (bar-close bar)
                                                (position-upper-limit pos)) 0)
                           (> (bar-close bar) (position-price pos)))
                      (begin
                        (close-proc pos-id (close-position (position-index pos) (bar-close bar)
                                                    'gain (- (bar-close bar) (position-price pos))))
                        (loop (cdr src) dest))
                      (loop (cdr src) (cons entry dest)))))))))

(define (inspect conn style date positions close-proc)
  (let* ((cur (currency-pair-name (trading-style-currency-pair style)))
         (bar-size (trading-style-bar-size style))
         (data (query-data conn cur date *data-count* bar-size))
         (index (bar-count conn cur date bar-size))
         (actual-date (last-date data)))
    (if (> (time-second (time-difference (date->time-utc date) (date->time-utc actual-date)))
           (if (string=? (trading-style-bar-size style) "1 hour")
               (* 65 60)
               (* 17 60)))
        (values #f positions)
        (let ((bar (last-bar data)))
          (let ((new-positions (update-position positions bar index close-proc)))
            (values (or (inspect-for-sell data index) (inspect-for-buy data index)) new-positions))))))
