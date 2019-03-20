(use gauche.threads)
(use gauche.collection)
(use rfc.http)

(use srfi-19)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)

(add-load-path "./lib/")
(use violet)

(use dbi)
(use dbd.pg)

;;
;; Application
;;

(define (get-random)
  (call-with-input-file "/dev/random"
    (^p
     (let* ((ch (read-char p))
            (result (if (char? ch)
                        (let ((num (char->integer ch)))
                          (thread-sleep! (/ num 1000))
                          num)
                        (x->string ch))))
       result))))

(define *conn* (dbi-connect "dbi:pg:user=postgres;host=localhost"))

(define query-data
  (let ((query (dbi-prepare *conn* "SELECT time, open, high, low, close FROM bars WHERE time >= to_timestamp(?) - interval '1' day and time < to_timestamp(?) and size = '1 hour' order by time")))
    (lambda (time)
      (let* ((sec (time->seconds time))
             (result (dbi-execute query sec sec))
             (getter (relation-accessor result)))
        (fold (lambda (row part)
                (let ((highest (car part))
                      (lowest (cadr part))
                      (rows (caddr part))
                      (high (string->number (getter row "high")))
                      (low (string->number (getter row "low"))))
                  (list
                   (max high highest)
                   (min low lowest)
                   (cons (list (date->time-utc (string->date (getter row "time") "~Y-~m-~d ~H:~M:~S"))
                               (string->number (getter row "open"))
                               (string->number (getter row "close"))
                               high
                               low)
                         rows))))
              '(0 9999999 ()) result)))))

(define *hour* (seconds->time 3600))
(define *day* (seconds->time (* 3600 24)))

(define (format-data data end-time)
  (let ((chart-height 500)
        (chart-width 500))
    `(,(let ((highest (car data))
             (lowest (cadr data))
             (rows (caddr data)))
         (let ((translate (^v (- chart-height (* (- v lowest) (/ chart-height (- highest lowest)))))))
           `(svg (@ (width chart-width) (height chart-height))
                 ,@(map (lambda (row)
                          (let ((time  (car row))
                                (open  (cadr row))
                                (close (caddr row))
                                (high  (cadddr row))
                                (low   (car (cddddr row))))
                            (let* ((t (time-difference time end-time))
                                   (hour (+ 24 (/ (time->seconds t) 3600)))
                                   (color (if (> open close) "red" "white")))
                              `(rect (@ (x ,(* hour 20))
                                        (y ,(translate (max open close)))
                                        (width 10)
                                        (height ,(abs (- (translate open) (translate close))))
                                        (style ,#`"fill:,color;stroke:black;stroke-width:1")))
                              )))
                        rows)))))))

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (let* ((time (date->time-utc (make-date 0 0 0 0 1 1 2019 0)))
              (data (await (^[] (query-data time)))))
         (respond/ok req `(sxml (html (body (h1 ,#`"USD.EUR ,(date->string (time-utc->date time))")
                                            ,@(format-data data time)
                                            )))))))))

#;(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (let* ((count (let ((n (await get-random))) (if (integer? n) (modulo n 10) 1)))
              (nums (let loop ((count count) (dest ()))
                      (if (zero? count)
                          dest
                          (loop (- count 1)
                                (cons (await get-random) dest))))))
         (respond/ok req `(sxml (html (body (h1 "Random Numbers")
                                            ,@(map (^n `(pre ,(x->string n))) nums)
                                            )))))))))

