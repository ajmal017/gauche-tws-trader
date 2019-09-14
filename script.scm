(use sxml.tools)
(use gauche.threads)
(use data.queue)
(use srfi-19)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)
(use ext.tws-client)

(add-load-path "./lib/")
(use violet)
(use trader)
(use query)
(use scanner)

(use redis)

(use config)

;;
;; Application
;;

(define *conn* (redis-open redis-host 6379))
#?=*conn*

(define (make-bar-from-row row chart-width half-bar-width bar-width count index transform-y)
  (let ((date  (bar-date  row))
        (open  (bar-open  row))
        (close (bar-close row))
        (high  (bar-high  row))
        (low   (bar-low   row)))
    (let* ((x (x->integer (* chart-width (/ index count))))
           (line-color (if (> open close) "red" "black"))
           (color (if (> open close) "red" "white")))
      (let ((bar `(a (@ (href ,#`"/,(date-year date)/,(date-month date)/,(date-day date)/,(date-hour date)/,(date-minute date)"))
                     (line (@ (x1 ,(+ x half-bar-width))
                              (y1 ,(transform-y high))
                              (x2 ,(+ x half-bar-width))
                              (y2 ,(transform-y low))
                              (style "stroke:black;stroke-width:0.5")))
                     (rect (@ (x ,x)
                              (y ,(transform-y (max open close)))
                              (width ,bar-width)
                              (height ,(abs (- (transform-y open)　(transform-y close))))
                              (style ,#`"fill:,color;stroke:,line-color;stroke-width:0.5")
                              )))))
        bar))))

(define (draw-line poly chart-width count transform half-bar-width color)
  (let ((a (poly-a poly))
        (b (poly-b poly))
        (c (poly-c poly))
        (x0 0)
        (x1 (- count 1)))
    (if (zero? a)
        `(line (@ (x1 0)
                  (y1 ,(- (/ c b)))
                  (x2 ,chart-width)
                  (y2 ,(- (/ c b)))
                  (style ,#`"stroke:,color;stroke-width:1")))
        `(line (@ (x1 ,(+ (x->integer (* chart-width (/ x0 count))) half-bar-width))
                  (y1 ,(transform (- (/ (+ (* a x0) c) b))))
                  (x2 ,(+ (x->integer (* chart-width (/ x1 count))) half-bar-width))
                  (y2 ,(transform (- (/ (+ (* a x1) c) b))))
                  (style ,#`"stroke:,color;stroke-width:1"))
               ))))

(define (format-data data)
  (let ((chart-height 500)
        (chart-width 1000))
    `(,(let ((highest (data-set-highest data))
             (lowest (data-set-lowest data))
             (rows (data-set-rows data))
             (count (data-set-count data)))
         (let ((transform-y
                (^v (- chart-height
                       (* (- v lowest)
                          (/ chart-height
                             (- highest lowest))
                          0.8)
                       (* chart-height 0.1)))))
           (let* ((step (/ chart-width count))
                  (bar-width (x->integer (/ step 2)))
                  (half-bar-width (x->integer (/ step 4)))
                  (draw-line* (^[poly col]
                                (draw-line poly chart-width count transform-y half-bar-width col))))
             `(svg (@ (width ,chart-width) (height ,chart-height))
                   (rect (@ (x ,(- chart-width (* 24 step)))
                            (y 0)
                            (width ,(* 24 step))
                            (height ,chart-height)
                            (style "stroke:none;fill:azure")))
                   (rect (@ (x ,(- chart-width (* 48 step)))
                            (y 0)
                            (width ,(* 24 step))
                            (height ,chart-height)
                            (style "stroke:none;fill:Lavender")))
                   ,@(let loop ((rows rows)
                                (index 0)
                                (dest ()))
                       (if (null? rows)
                           (reverse dest)
                           (let ((row (car rows)))
                             (let ((bar (make-bar-from-row row chart-width half-bar-width bar-width
                                                           count index transform-y)))
                               (loop (cdr rows) (+ 1 index) (cons bar dest))))))
                   ,(draw-line* (min-line/range/step data 0            (- count 48) 4) "black")
                   ,(draw-line* (min-line/range      data (- count 48) 23)             "black")
                   ,(draw-line* (max-line/range/step data 0            (- count 48) 4) "blue")
                   ,(draw-line* (max-line/range      data (- count 48) 23)             "blue")
                   )))))))

(define (create-page . children)
  `(html
     (@ (lang "en"))
     (head
      (meta (@ (charset "utf-8")))
      (meta (@ (name "viewport") (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
      (meta (@ (name "description") (content "")))
      (meta (@ (name "author") (content "Mark Otto, Jacob Thornton, and Bootstrap contributors")))
      (title "Starter Template · Bootstrap")
      (link (@
               (rel "stylesheet")
               (integrity "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T")
               (href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")
               (crossorigin "anonymous")))
      (style
          (string-append
           ".bd-placeholder-img {"
           "  font-size: 1.125rem;"
           "  text-anchor: middle;"
           "  -webkit-user-select: none;"
           "  -moz-user-select: none;"
           "  -ms-user-select: none;"
           "  user-select: none;"
           "}"
           "@media (min-width: 768px) {"
           "  .bd-placeholder-img-lg {"
           "    font-size: 3.5rem;"
           "  }"
           "}"
           ))
      (link (@ (rel "stylesheet") (href "/static/starter-template.css"))))
     (body
      (nav (@ (class "navbar navbar-expand-md navbar-dark bg-dark fixed-top"))
             (a (@ (href "#") (class "navbar-brand")) "Trader")
             (button
              (@
               (type "button")
               (data-toggle "collapse")
               (data-target "#navbarsExampleDefault")
               (class "navbar-toggler")
               (aria-label "Toggle navigation")
               (aria-expanded "false")
               (aria-controls "navbarsExampleDefault"))
              (span (@ (class "navbar-toggler-icon"))))
             (div (@ (id "navbarsExampleDefault") (class "collapse navbar-collapse"))
                    (ul (@ (class "navbar-nav mr-auto"))
                          (li (@ (class "nav-item active"))
                                (a (@ (href "#") (class "nav-link"))
                                     "Home " (span (@ (class "sr-only")) "(current)")))
                          (li (@ (class "nav-item")) (a (@ (href "#") (class "nav-link")) "Link"))
                          (li (@ (class "nav-item"))
                                (a (@
                                      (tabindex "-1") (href "#") (class "nav-link disabled")
                                      (aria-disabled "true"))
                                     "Disabled"))
                          (li (@ (class "nav-item dropdown"))
                                (a (@ (id "dropdown01") (href "#")
                                      (data-toggle "dropdown")
                                      (class "nav-link dropdown-toggle")
                                      (aria-haspopup "true")
                                      (aria-expanded "false"))
                                     "Dropdown")
                                (div (@ (class "dropdown-menu") (aria-labelledby "dropdown01"))
                                       (a (@ (href "#") (class "dropdown-item")) "Action")
                                       (a (@ (href "#") (class "dropdown-item")) "Another action")
                                       (a (@ (href "#") (class "dropdown-item")) "Something else here"))))
                    (form
                     (@ (class "form-inline my-2 my-lg-0"))
                     (input (@ (type "text") (placeholder "Search") (class "form-control mr-sm-2")
                               (aria-label "Search")))
                     (button (@ (type "submit") (class "btn btn-secondary my-2 my-sm-0"))
                               "Search"))))
      (main
       (@ (role "main") (class "container"))
       ,@children)
      (script (@
                 (src "https://code.jquery.com/jquery-3.3.1.slim.min.js")
                 (integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo")
                 (crossorigin "anonymous"))
              "")
      (script (@
               (src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js")
               (integrity "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1")
               (crossorigin "anonymous"))
              "")
      (script (@
               (src "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js")
               (integrity "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM")
               (crossorigin "anonymous"))
              "")))
  )

(define (next-day date)
  (let* ((time (date->time-utc date))
         (a-day (make-time time-duration 0 (* 24 60 60)))
         (next-day-time (add-duration time a-day)))
    (time-utc->date next-day-time)))

(define-http-handler #/^\/(\d+)\/0*(\d+)\/0*(\d+)\/0*(\d+)\/0*(\d+)\/?/
  (^[req app]
    (let-params req ([year   "p:1" :convert x->integer]
                     [month  "p:2" :convert x->integer]
                     [date   "p:3" :convert x->integer]
                     [hour   "p:4" :convert x->integer]
                     [minute "p:5" :convert x->integer])
      (violet-async
       (^[await]
         (let* ((end-date (make-date 0 0 minute hour date month year 0))
                (data (await (^[] (query-data *conn* "EUR.GBP" (next-day end-date)
                                              (* 24 5 4) "1 hour")))))
           (respond/ok req (cons "<!DOCTYPE html>"
                                 (sxml:sxml->html
                                  (create-page
                                   `(html (body (p ,#`"EUR.GBP ,(date->string end-date)")
                                                (h2 "1 hour")
                                                (div ,@(format-data data))
                                                ))))))))))))

(define-http-handler "/"
  (^[req app]
    (respond/redirect req "/2019/1/1/0/0") ))

(define-http-handler #/^\/static\// (file-handler))

;;;;;;;;;;;;;;

(define tws (make-tws-client))

(define *request-id* 4000)
(define (request-id!)
  (let ((id *request-id*))
    (inc! *request-id*)
    id))

(tws-client-connect tws tws-host 7497 0)
(define *order-id* #f)
(define (order-id!)
  (let ((id #?=*order-id*))
    (inc! *order-id*)
    id))

(define *eur-gbp* (make-currency-pair "EUR" "GBP"))

(define *eur-gbp-1hour*
  (make-trading-style
   *eur-gbp*
   "IDEALPRO"
   "1 hour"
   "3600 S"
   "3660 S"
   "1 Y"
   "4 W"
   ))

(define (on-next-valid-id id)
  (set! *order-id* id)
  (let* ((date
          (let ((cur (current-date)))
            (make-date 0 0 0
                       (date-hour cur) (date-day cur) (date-month cur) (date-year cur)
                       (date-zone-offset cur))))
         (date-str (date->string date "~Y~m~d ~T"))
         (last-data #?=(query-data *conn* (currency-pair-name
                                           (trading-style-currency-pair *eur-gbp-1hour*))
                                   date 1 (trading-style-bar-size *eur-gbp-1hour*)))
         (duration
          (if (zero? #?=(data-set-count last-data))
              (trading-style-history-period *eur-gbp-1hour*)
              (let ((sec
                     (time-second
                      (time-difference
                       (date->time-utc date)
                       (date->time-utc (bar-date (car (data-set-rows last-data))))))))
                (if (> sec 86400)
                    (trading-style-min-period *eur-gbp-1hour*)
                    #`",sec S")))))
    (if (string=? #?=duration (trading-style-duration-for-wait *eur-gbp-1hour*))
        (sleep-and-update)
        (enqueue! *task-queue*
                  (lambda ()
                    (tws-client-historical-data-request
                     tws (request-id!)
                     (currency-pair-symbol (trading-style-currency-pair *eur-gbp-1hour*))
                     "CASH"
                     (currency-pair-currency (trading-style-currency-pair *eur-gbp-1hour*))
                     (trading-style-exchange *eur-gbp-1hour*)
                     date-str
                     duration
                     (trading-style-bar-size *eur-gbp-1hour*)
                     "MIDPOINT"))))))

(define (on-historical-data req-id time open high low close volume count wap)
  (let ((date (string->date time "~Y~m~d  ~H:~M:~S"))) ; "20190830  22:00:00"
    (add-data *conn*
              (currency-pair-name (trading-style-currency-pair *eur-gbp-1hour*))
              (trading-style-bar-size *eur-gbp-1hour*)
              date open close high low)))

(define *task-queue* (make-mtqueue))

(define (update-history style)
  (let* ((date
          (let ((cur #?=(current-date)))
            (make-date 0 0 0
                       (date-hour cur) (date-day cur)
                       (date-month cur) (date-year cur)
                       (date-zone-offset cur))))
         (date-str (date->string date "~Y~m~d ~T")))
    (tws-client-historical-data-request
     tws #?=(request-id!)
     (currency-pair-symbol (trading-style-currency-pair style))
     "CASH"
     (currency-pair-currency (trading-style-currency-pair style))
     (trading-style-exchange style)
     date-str
     (trading-style-duration-for-query style)
     (trading-style-bar-size style)
     "MIDPOINT")
    ))

(define (sleep-and-update)
  (let ((min (date-minute (current-date))))
    (thread-start!
     (make-thread
      (lambda ()
        (sys-sleep #?=(* 60 (- 60 min)))
        #?="slept"
        (enqueue! *task-queue*
                  (lambda ()
                    (update-history *eur-gbp-1hour*))))))))

(define *positions* ())
(define (position-id)
  (redis-get *conn* "position-id"))

(define (position-id-bump!)
  #?=(redis-incr *conn* "position-id"))

(define *quantitiy-unit* 20000.0)       ; minimum size = 20K

(define (close-position close-order)
  #?=close-order
  ;;; (list 'close pos-idx price result gain)
  (let ((pos (get-position (cadr close-order))))
    (order (case (position-action pos)
             ((sell) "BUY")
             ((buy) "SELL"))
           (currency-pair-symbol (trading-style-currency-pair *eur-gbp-1hour*))
           (currency-pair-currency (trading-style-currency-pair *eur-gbp-1hour*))
           (trading-style-exchange *eur-gbp-1hour*)
           *quantitiy-unit*)
    ))

(define (get-position id)
  (let ((pos-str (redis-hget *conn* "positions" id)))
    (apply make-position #?=(read-from-string pos-str))))

(define (open-position pos)
  #?=(position->string pos)

  (order (case (position-action pos)
           ((sell) "SELL")
           ((buy) "BUY"))
         (currency-pair-symbol (trading-style-currency-pair *eur-gbp-1hour*))
         (currency-pair-currency (trading-style-currency-pair *eur-gbp-1hour*))
         (trading-style-exchange *eur-gbp-1hour*)
         *quantitiy-unit*
         ))

(define (order action symbol currecy exchange quantity)
  (enqueue! *task-queue*
            (lambda ()
              (tws-client-place-fx-market-order
               tws #?=(order-id!) #?=symbol currecy exchange
               #?=action
               quantity))))

(define (on-historical-data-end req-id start-date end-date)
  #?=`(,req-id ,start-date ,end-date)
  (let-values (((pos poss) (inspect *conn* (current-date) *positions* (position-id) close-position)))
    (if pos
        (begin
          (set! *positions* (cons pos poss))
          (open-position pos)
          (position-id-bump!))
        (set! *positions* poss)))
  #?=*positions*

  (sleep-and-update)
)

(thread-start! 
 (make-thread
  (lambda ()
    (let loop ()
      (let task-loop ()
        (let ((task (dequeue! *task-queue* #f)))
          (when task
            (task)
            (task-loop))))
      (tws-client-process-messages tws)
      (loop)))))
