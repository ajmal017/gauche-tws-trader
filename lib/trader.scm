(define-module trader
  (use gauche.collection)
  (use gauche.record)
  (use srfi-19)

  (use util.match)

  (export
   min-line/range/step
   min-line/range
   max-line/range/step
   max-line/range
   bar-date
   bar-low
   bar-high
   bar-open
   bar-close
   make-poly
   poly?
   poly-a
   poly-b
   poly-c
   serialize-poly
   distance-to-line
   make-data-set
   data-set-highest
   data-set-lowest
   data-set-count
   data-set-rows
   make-bar
   make-position
   position-index
   position-date
   position-action
   position-price
   position-upper-limit
   position-lower-limit
   serialize-position
   position-info
   deserialize-position
   make-pos-info
   pos-info-gain
   pos-info-long-trend-poly
   pos-info-long-trend-error
   pos-info-short-trend-poly
   pos-info-short-trend-error
   make-currency-pair
   currency-pair-name
   currency-pair-symbol
   currency-pair-currency
   make-trading-style
   trading-style-currency-pair
   trading-style-exchange
   trading-style-bar-size
   trading-style-duration-for-wait
   trading-style-duration-for-query
   trading-style-history-period
   trading-style-min-period
   ))

(select-module trader)

(define-record-type data-set #t #t
  highest
  lowest
  rows
  count)

(define-record-type bar #t #t
  date
  open
  close
  high
  low)

(define-record-type poly #t #t a b c)

(define (serialize-poly poly)
  (list 'poly (poly-a poly) (poly-b poly) (poly-c poly)))

(define (make-line-poly x0 y0 x1 y1)
  ;; returns polynomial in ax + by + c = 0
  (if (= y0 y1)
      (make-poly 0 1 (- y0))
      (let* ((b (- (/ (- x0 x1) (- y0 y1))))
             (c (- (+ x0 (* b y0)))))
        (when (or (nan? b) (nan? c) (infinite? b) (infinite? c))
          #?=(list b c x0 y0 x1 y1)
          (error "invalid!"))
        (make-poly 1 b c))))

(define (distance-to-line x y poly)
  (let ((a (poly-a poly))
        (b (poly-b poly))
        (c (poly-c poly)))
    (- y (- (/ (+ (* a x) c) b)))))

(define (line-from-rows rows pick filter accum step)
  (define (kdr lst)
    (let loop ((lst lst)
               (step step))
      (if (or (zero? step) (null? lst))
          lst
          (loop (cdr lst) (- step 1)))))

  (let loop ((rows1 rows)
             (line #f)
             (min-total-distance +inf.0)
             (x0 0))
    (if (null? rows1)
        (begin
          (when (not line)
            #?=step
            (print (map pick rows))
            (print (bar-date (last rows)))
            #?=min-total-distance)
          (values line min-total-distance)
          )
        (let* ((low-first (pick (car rows1))))
          (let loop2 ((rest (kdr rows1))
                      (min-line-poly line)
                      (min-total-distance min-total-distance)
                      (x1 (+ x0 step)))
            (if (null? rest)
                (loop (kdr rows1) min-line-poly min-total-distance (+ x0 step))
                (let ((line-poly (make-line-poly x0 low-first
                                                 x1 (pick (car rest)))))
                  (let loop3 ((rows rows)
                              (total-distance 0)
                              (count 0))
                    (if (null? rows)
                        (begin
                          (if (> min-total-distance total-distance)
                              (loop2 (kdr rest) line-poly     total-distance     (+ x1 step))
                              (loop2 (kdr rest) min-line-poly min-total-distance (+ x1 step)))
                        )
                        (let* ((distance (distance-to-line count (pick (car rows)) line-poly)))
                          (if (or (= count x0) (= count x1))
                              (loop3 (kdr rows) total-distance (+ count step))
                              (if (filter distance)
                                  (loop3 (kdr rows) (accum total-distance distance) (+ count step))
                                  (begin
                                    #;(print (list 'skipping x0 x1 count distance))
                                    (loop2 (kdr rest) min-line-poly min-total-distance (+ x1 step)))
                                  )
                              )))))))))))

(define (splice-data data offset length)
  (let ((list (data-set-rows data))
        (total-length (data-set-count data)))
    (take (take-right list (- total-length offset)) length)))

(define (square-add a b)
  (let ((dest (+ a (* b b))))
    (when (nan? dest)
      #?=(cons a b)
      (error "NAN!"))
    dest))

(define (min-line/range/step data offset points step)
  (receive (line distance)
      (line-from-rows (splice-data data offset points)
                      bar-low (^x (not (negative? x))) + step)
    (values (offset-line line offset) distance)))

(define (max-line/range/step data offset points step)
  (receive (line distance)
      (line-from-rows (splice-data data offset points)
                      bar-high (^x (not (positive? x))) - step)
    (values (offset-line line offset) distance)))

(define (min-line/range data offset points)
  (receive (line distance) (min-line/range/step data offset points 1)
    (values line distance)))

(define (max-line/range data offset points)
  (receive (line distance) (max-line/range/step data offset points 1)
    (values line distance)))

(define (offset-line poly offset-x)
  (let ((a (poly-a poly))
        (b (poly-b poly))
        (c (poly-c poly)))
    (if (zero? a)
        poly
        (make-poly a b (- c offset-x)))))

(define-record-type position #t #t
  index
  date
  action
  price
  upper-limit
  lower-limit
  info)

(define (serialize-position pos)
  (define (serialize-limit lim)
    (if (poly? lim)
        (serialize-poly lim)
        lim))

  (list 'position
        (position-index pos)
        (date->string (position-date pos) "~4")
        (position-action pos)
        (position-price pos)
        (serialize-limit (position-upper-limit pos))
        (serialize-limit (position-lower-limit pos))
        (serialize-pos-info (position-info pos))))

(define (deserialize-position ser)
  (define (deserialize-limit lim)
    (match lim
           ((? number? num) num)
           (('poly a b c) (make-poly a b c))))

  (match ser
         (('position pos-id
                     date-str
                     action
                     price
                     upper-limit
                     lower-limit
                     info)
          (make-position pos-id (string->date date-str "~Y-~m-~dT~H:~M:~S~z") action price
                         (deserialize-limit upper-limit)
                         (deserialize-limit lower-limit)
                         (deserialize-pos-info info)))))

(define (serialize-pos-info info)
  (list 'pos-info
        (pos-info-gain info)
        (serialize-poly (pos-info-long-trend-poly info))
        (pos-info-long-trend-error info)
        (serialize-poly (pos-info-short-trend-poly info))
        (pos-info-short-trend-error info)))

(define (deserialize-pos-info ser)
  (match ser
         (('pos-info (? number? gain)
                     ('poly a1 b1 c1)
                     (? number? err1)
                     ('poly a2 b2 c2)
                     (? number? err2))
          (make-pos-info gain
                         (make-poly a1 b1 c1) err1
                         (make-poly a2 b2 c2) err2))))

(define-record-type pos-info #t #t
  gain
  long-trend-poly
  long-trend-error
  short-trend-poly
  short-trend-error)

(define-record-type result #t #t
  closed-at
  position
  gain)

(define-record-type currency-pair #t #t
  symbol
  currency
  )

(define (currency-pair-name cp)
  #`",(currency-pair-symbol cp).,(currency-pair-currency cp)")

(define-record-type trading-style #t #t
  currency-pair
  exchange
  bar-size
  duration-for-wait
  duration-for-query
  history-period
  min-period
  )
