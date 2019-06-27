(define-module trader
  (use gauche.collection)
  (use gauche.record)
  (use srfi-19)

  (use dbi)
  (use dbd.pg)

  (export
   query-data
   min-line/range/step
   min-line/range
   max-line/range/step
   max-line/range
   bar-date
   bar-low
   bar-high
   bar-open
   bar-close
   poly-a
   poly-b
   poly-c
   distance-to-line
   data-set-highest
   data-set-lowest
   data-set-count
   data-set-rows
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

(define (query-data conn end-date count size)
  (let ((query (dbi-prepare conn "SELECT DISTINCT time, open, high, low, close FROM bars WHERE time <= to_timestamp(?) and size = ? order by time desc limit ?")))
    (let* ((end-sec (time->seconds (date->time-utc end-date)))
           (result (dbi-execute query end-sec size count))
           (getter (relation-accessor result))
           (dest (fold (lambda (row part)
                         (let ((highest (data-set-highest part))
                               (lowest (data-set-lowest part))
                               (rows (data-set-rows part))
                               (count (data-set-count part))
                               (high (string->number (getter row "high")))
                               (low (string->number (getter row "low"))))
                           (make-data-set
                            (max high highest)
                            (min low lowest)
                            (cons (make-bar
                                   (string->date (getter row "time") "~Y-~m-~d ~H:~M:~S")
                                   (string->number (getter row "open"))
                                   (string->number (getter row "close"))
                                   high
                                   low)
                                  rows)
                            (+ 1 count))))
                       (make-data-set 0 9999999 () 0) result)))
      dest)))

;; (define (low-of row) (car (cddddr row)))
;; (define (high-of row) (cadddr row))

(define-record-type poly #t #t a b c)

#;(define (make-poly a b c)
  (list a b c))

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
        (values line min-total-distance)
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
                          (list x0 x1 total-distance min-total-distance)
                        (if (> min-total-distance total-distance)
                            (loop2 (kdr rest) line-poly     total-distance     (+ x1 step))
                            (loop2 (kdr rest) min-line-poly min-total-distance (+ x1 step)))
                        )
                        (let* ((distance (distance-to-line count (pick (car rows)) line-poly)))
                          (if (or (= count x0) (= count x1))
                              (loop3 (kdr rows) total-distance (+ count step))
                              (if (filter distance)
                                  (loop3 (kdr rows) (accum total-distance distance) (+ count step))
                                  (loop2 (kdr rest) min-line-poly min-total-distance (+ x1 step)))
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
                           bar-low positive? square-add step)
           (values (offset-line line offset) distance)))

(define (max-line/range/step data offset points step)
  (receive (line distance)
           (line-from-rows (splice-data data offset points)
                           bar-high negative? square-add step)
           (values (offset-line line offset) distance)))

(define (min-line/range data offset points)
  (receive (line distance) (min-line/range/step data offset points 1)
           (values line distance)))

(define (max-line/range data offset points)
  (receive (line distance) (max-line/range/step data offset points 1)
           (values line distance)))

;; (define poly-a car)
;; (define poly-b cadr)
;; (define poly-c caddr)

(define (offset-line poly offset-x)
  (let ((a (poly-a poly))
        (b (poly-b poly))
        (c (poly-c poly)))
    (if (zero? a)
        poly
        (make-poly a b (- c offset-x)))))
