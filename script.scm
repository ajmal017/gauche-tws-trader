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

(define *conn* (dbi-connect "dbi:pg:user=postgres;host=britney.local"))

(define query-data
  (let ((query (dbi-prepare *conn* "SELECT time, open, high, low, close FROM bars WHERE time >= to_timestamp(?) and time < to_timestamp(?) and size = '1 hour' order by time")))
    (lambda (begin-time end-time)
      (let* ((begin-sec (time->seconds begin-time))
             (end-sec (time->seconds end-time))
             (result (dbi-execute query begin-sec end-sec))
             (getter (relation-accessor result)))
        (fold (lambda (row part)
                (let ((highest (car part))
                      (lowest (cadr part))
                      (rows (caddr part))
                      (count (cadddr part))
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
                         rows)
                   (+ 1 count))))
              '(0 9999999 () 0) result)))))

(define *hour* (seconds->time 3600))
(define *day* (seconds->time (* 3600 24)))

(define (format-data data begin-time end-time)
  (let ((chart-height 500)
        (chart-width 500)
        (duration (time->seconds (time-difference end-time begin-time))))
    `(,(let ((highest (car data))
             (lowest (cadr data))
             (rows (caddr data))
             (count (cadddr data)))
         (let ((translate (^v (- chart-height
                                 (* (- v lowest) (/ chart-height (- highest lowest)))))))
           (let* ((step (/ chart-width count))
                  (bar-width (x->integer (/ step 2)))
                  (half-bar-width (x->integer (/ step 4))))
             `(svg (@ (width ,chart-width) (height ,chart-height))
                   ,@(map (lambda (row)
                            (let ((time  (car row))
                                  (open  (cadr row))
                                  (close (caddr row))
                                  (high  (cadddr row))
                                  (low   (car (cddddr row))))
                              (let* ((t (time->seconds (time-difference time begin-time)))
                                     (x (x->integer (* chart-width (/ t duration))))
                                     (color (if (> open close) "red" "white")))
                                (list `(line (@ (x1 ,(+ x half-bar-width)) (y1 ,(translate high))
                                                (x2 ,(+ x half-bar-width)) (y2 ,(translate low))
                                                (style "stroke:black;stroke-width:2")))
                                      `(rect (@ (x ,x)
                                                (y ,(translate (max open close)))
                                                (width ,bar-width)
                                                (height ,(abs (- (translate open)
                                                                 (translate close))))
                                                (style ,#`"fill:,color;stroke:black;stroke-width:1"))))
                                )))
                          rows))))))))

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (let* ((begin-time (date->time-utc (make-date 0 0 0 0 31 12 2018 0)))
              (end-time (date->time-utc (make-date 0 0 0 0 1 1 2019 0)))
              (data (await (^[] (query-data begin-time end-time)))))
         (respond/ok req `(sxml (html (body (h1 ,#`"USD.EUR ,(date->string (time-utc->date begin-time))")
                                            ,@(format-data data begin-time end-time)
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

