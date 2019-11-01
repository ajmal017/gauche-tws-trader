(use srfi-19)
(use util.match)

(add-load-path "./lib/")
(use trader)

(define *positions* (make-hash-table))

(define *gain-thresholds* '(0.01 0.02 0.03 0.04 0.05))
(define *total-with-thresholds* (make-hash-table))

(define *id* 0)
(define *id-offset* 0)
(define (get-id!)
  (inc! *id*)
  *id*)

(define (new-currency!)
  (set! *id-offset* (+ *id-offset* 10000)))

(define (print-poly id poly)
  (let* ((a (poly-a poly))
         (b (poly-b poly))
         (c (poly-c poly)))
    (print #`"INSERT INTO polies (id, a, b, c) VALUES (,|id|, ,|a|, ,|b|, ,|c|);")))

(define (eat-position dat)
  (let* ((pos (deserialize-position dat))
         (pos-id (+ *id-offset* (position-index pos)))
         (info (position-info pos))
         (upper (position-upper-limit pos))
         (lower (position-lower-limit pos))
         (poly-id (get-id!))
         (info-id (get-id!))
         (long-poly-id (get-id!))
         (short-poly-id (get-id!)))
    ;; polies
    (let ((poly (case (position-action pos) ((sell) lower) ((buy) upper)))
          (long-poly (pos-info-long-trend-poly info))
          (short-poly (pos-info-short-trend-poly info)))
      (print-poly poly-id poly)
      (print-poly long-poly-id long-poly)
      (print-poly short-poly-id short-poly))

    ;; info
    (let ((gain (pos-info-gain info))
          (long-error (pos-info-long-trend-error info))
          (short-error (pos-info-short-trend-error info)))
      (print "INSERT INTO position_infos "
             "(id, expected_gain, "
             "long_trend_poly, long_trend_error, short_trend_poly, short_trend_error) VALUES "
             #`"(,|info-id|, ,|gain|, "
             #`",|long-poly-id|, ,|long-error|, ,|short-poly-id|, ,|short-error|);"))

    ;; open
    (let ((date (date->string (position-date pos) "~Y-~m-~d ~H:~M:~S~z"))
          (action (symbol->string (position-action pos)))
          (price (position-price pos))
          (stop-loss (case (position-action pos)
                       ((sell) (position-upper-limit pos))
                       ((buy) (position-lower-limit pos)))))
      (print "INSERT INTO opens "
             "(id, created_at, action, expected_price, stop_loss, take_profit_poly, info) VALUES "
             #`"(,|pos-id|, ',|date|', ',|action|', ,|price|, "
             #`",|stop-loss|, ,|poly-id|, ,|info-id|);"))

    (hash-table-put! *positions* (position-index pos) pos)
    ))

(define (eat-close id total pos-idx-close date-str pos-idx price result gain)
  (let* ((date (string->date date-str "~Y-~m-~dT~H:~M:~S~z"))
         (date-sql (date->string date "~Y-~m-~d ~H:~M:~S~z")))
    (print "INSERT INTO closes (id, created_at, open, price, gain) VALUES "
           #`"(,(+ *id-offset* id), ',|date-sql|', ,(+ *id-offset* pos-idx), ,|price|, ,|gain|);")))

(define (main . args)
  (print "BEGIN;")
  (let loop ((total-gain 0))
    (let ((line (read)))
      (unless (eof-object? line)
              (match line
                     (('position _ ...)
                      (eat-position line)
                      (loop total-gain))
                     (('close id total pos-idx-close date ('close pos-idx price result gain))
                      (eat-close id total pos-idx-close date pos-idx price result gain)
                      (loop (+ total-gain gain)))
                     (('scan _ _)
                      (new-currency!)
                      (loop total-gain))
                     (else (loop total-gain))))))
  (print "COMMIT;"))
