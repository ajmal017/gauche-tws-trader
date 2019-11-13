(use sxml.tools)
(use gauche.threads)
(use gauche.record)
(use data.queue)
(use srfi-19)
(use scheme.vector)
(use util.match)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)
(use ext.tws-client)

(add-load-path "./lib/")
(use violet)
(use trader)
(use query)
(use scanner)
(use position)
(use logger)

(use redis)

(use config)

;;
;; Application
;;

(define *conn* (redis-open redis-host redis-port))
#?=*conn*

(define (make-bar-from-row base-url row chart-width half-bar-width bar-width count index transform-y)
  (let ((date  (bar-date  row))
        (open  (bar-open  row))
        (close (bar-close row))
        (high  (bar-high  row))
        (low   (bar-low   row)))
    (let* ((x (x->integer (* chart-width (/ index count))))
           (line-color (if (> open close) "red" "black"))
           (color (if (> open close) "red" "white")))
      (let ((bar `(a (@ (href ,#`",base-url,(date-year date)/,(date-month date)/,(date-day date)/,(date-hour date)/,(date-minute date)"))
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

(define (date<? a b)
  (time<? (date->time-utc a) (date->time-utc b)))

(define (draw-orders history prev-date date
                     chart-width width count index transform-y)
  (if prev-date
      (let loop ((key (tree-map-ceiling history prev-date))
                 (dest ()))
        (if (and key (date<? key date))
            (let ((order (tree-map-get history key)))
              (match order
                     ((action price)
                      (let ((x (x->integer (* chart-width (/ index count))))
                            (y (transform-y price))
                            (color (case action ((buy) "green") ((sell) "red"))))
                        (debug-log key action price)
                        (let ((elem `(rect (@ (x ,x)
                                              (y ,(- y (/ width 2)))
                                              (width ,width) (height ,width)
                                              (style ,#`"fill:,color;stroke:,color")))))
                          (loop (tree-map-successor history key) (cons elem dest)))))))
            dest))
      ()))

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

(define (format-data data history base-url)
  (let ((chart-height 500)
        (chart-width 1000))
    `(,(let ((highest (data-set-highest data))
             (lowest (data-set-lowest data))
             (rows (data-set-rows data))
             (count (data-set-count data)))
         (let ((transform-y
                (^v (let ((y (- chart-height
                                (* (- v lowest)
                                   (/ chart-height
                                      (- highest lowest))
                                   0.8)
                                (* chart-height 0.1))))
                      (x->integer y)))))
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
                                (dest ())
                                (prev-date #f))
                       (if (null? rows)
                           (reverse dest)
                           (let ((row (car rows)))
                             (let* ((bar (make-bar-from-row base-url
                                                            row chart-width half-bar-width bar-width
                                                            count index transform-y))
                                    (date (bar-date row))
                                    (orders (draw-orders history prev-date date
                                                         chart-width 10 count index transform-y)))
                               (loop (cdr rows) (+ 1 index)
                                     (cons (cons bar orders) dest) date)))))
                   ;; ,(draw-line* (min-line/range/step data 0            (- count 48) 4) "black")
                   ;; ,(draw-line* (min-line/range      data (- count 48) 23)             "black")
                   ;; ,(draw-line* (max-line/range/step data 0            (- count 48) 4) "blue")
                   ;; ,(draw-line* (max-line/range      data (- count 48) 23)             "blue")
                   )))))))

(define (create-page . children)
  `(html
     (@ (lang "en"))
     (head
      (meta (@ (charset "utf-8")))
      (meta (@ (name "viewport") (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
      (meta (@ (name "description") (content "")))
      (meta (@ (name "author") (content "Toru")))
      (title "Trader")
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
             (a (@ (href "/") (class "navbar-brand")) "Trader")
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
                                (a (@ (href "/") (class "nav-link"))
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

(define-http-handler #/^\/(\w+)\.(\w+)\/(15 mins)\/(\d+)\/0*(\d+)\/0*(\d+)\/0*(\d+)\/0*(\d+)\/?/
  (^[req app]
    (let-params req ([sym    "p:1"]
                     [cur    "p:2"]
                     [size   "p:3"]
                     [year   "p:4" :convert x->integer]
                     [month  "p:5" :convert x->integer]
                     [date   "p:6" :convert x->integer]
                     [hour   "p:7" :convert x->integer]
                     [minute "p:8" :convert x->integer])
      (violet-async
       (^[await]
         (let* ((symbol #`",|sym|.,|cur|")
                (end-date (make-date 0 0 minute hour date month year 0))
                (data (await (^[] (query-data *conn* symbol end-date
                                              (* 24 5 4) size))))
                (history (await (^[] (result-log->history-tree
                                      (filter-log/currency-pair *conn* sym cur 100))))))
           (respond/ok req (cons "<!DOCTYPE html>"
                                 (sxml:sxml->html
                                  (create-page
                                   `(html (body (p ,#`",symbol ,(date->string end-date)")
                                                (h2 ,size)
                                                (div ,@(format-data data history
                                                                    #`"/,|symbol|/,|size|/"))
                                                ))))))))))))

(define (render-positions cur-pair positions bar-size)
  (let ((cur (currency-pair-name cur-pair)))
    (map
     (^e (let* ((pos-id (car e))
                (p (cdr e))
                (key (entry-price-key cur-pair))
                (actual-price (redis-hget *conn* key pos-id))
                (date (position-date p))
                (action (position-action p))
                (url (date->string date #`"/,|cur|/,|bar-size|/~Y/~m/~d/~H/~M")))
             `(tr
               ((td ,cur)
               (td ,pos-id)
               (td (a (@ (href ,url)) ,(date->string date "~4")))
               (td ,(symbol->string action))
               (td ,(number->string (position-price p)))
               (td ,actual-price))
               (td ,(number->string (case action
                                      ((buy)  (position-lower-limit p))
                                      ((sell) (position-upper-limit p)))))
               (td (@ (style "padding: 0px; vertical-align: middle"))
                   (form (@ (action ,#`"/close/,pos-id") (method "post") (class "form-inline"))
                         (button (@ (type "submit") (class "btn btn-primary btn-sm")
                                    #;(disabled "disabled"))
                                 "Close Now")
                         )))))
     positions)))

(define (render-results num)
  (let ((logs (redis-zrevrange *conn* "result-log" 0 num)))
    (map
     (lambda (log)
       (let* ((log-pos (assoc 'position log))
              (pos (and log-pos (deserialize-position (cdr log-pos))))
              (stop-loss (and pos
                              (case (position-action pos)
                                ((sell) (position-upper-limit pos))
                                ((buy) (position-lower-limit pos)))))
              (expected-loss (if pos
                                 (format #f "~5,2f"
                                         (* 10000 (abs (- stop-loss (position-price pos)))))
                                 "--"))
              (close-order (assoc-ref log 'close-order)))
       `(tr
         (td ,(cdr (assoc 'pos-id      log)))
         (td ,(case (cdr (assoc 'action log))
                ((buy) "long")
                ((sell) "short")))
         (td ,(string-append (cdr (assoc 'sym log)) "." (cdr (assoc 'cur log))))
         (td ,(format #f "~10,5f (~,5f)"
                      (assoc-ref log 'open-price)
                      (if pos (position-price pos) 0)))
         (td ,(format #f "~10,5f (~,5f)"
                      (assoc-ref log 'close-price)
                      (if close-order (caddr close-order) 0)))
         ;; (td ,(format #f "~10,6f" (cdr (assoc 'gain        log))))
         (td ,(format #f "~10,2f" (cdr (assoc 'net-gain    log))))
         (td ,expected-loss))))
     (vector-fold-right (^[a b] (cons (read-from-string b) a)) '() logs))))

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (let ()
         (respond/ok req (cons "<!DOCTYPE html>"
                               (sxml:sxml->html
                                (create-page
                                 '(h2 "Positions")
                                 `(table (@ (class "table"))
                                   (tr (th "symbol")
                                       (th "position ID")
                                       (th "date")
                                       (th "action")
                                       (th "price")
                                       (th "actual price")
                                       (th "stop loss")
                                       (td ""))
                                   ,(map
                                     (lambda (style)
                                       (let* ((cur-pair (trading-style-currency-pair style))
                                              (positions (await (^[] #?=(get-all-positions style)))))
                                         (render-positions cur-pair positions "15 mins")))
                                     *trading-styles*))
                                 '(h2 "Results")
                                 `(table (@ (class "table"))
                                         (tr (th "pos ID")
                                             (th "type")
                                             (th "symbol")
                                             (th "open")
                                             (th "close")
                                             ;; (th "gain")
                                             (th "net gain")
                                             (th "exp. loss"))
                                         ,(render-results 100))
                                 )))))))))

(define-http-handler #/^\/close\/(\d+)/
  (^[req app]
    (let-params req ([pos-id "p:1"])
                (violet-async
                 (^[await]
                   (await
                    (^[]
                      (enqueue! *task-queue*
                                (^[]
                                  (close-position pos-id `(close ,pos-id _ manual))
                                  ))))
                   (thread-sleep! 3)
                   (respond/redirect req "/")
                   )))))

(define-http-handler #/^\/static\// (file-handler))

;;;;;;;;;;;;;;

(define *tws* (make-tws-client))

(define *request-id* 4000)
(define (request-id!)
  (let ((id *request-id*))
    (inc! *request-id*)
    id))

(tws-client-connect *tws* tws-host tws-port tws-client-id)
(define *order-id* #f)
(define (order-id!)
  (let ((id *order-id*))
    (inc! *order-id*)
    id))

(define (fifteen-min-style curpair)
  (make-trading-style
   curpair
   "SMART"
   "15 mins"
   "900 S"
   "960 S"
   "3 M"
   "1 W"
   ))

;; (define *eur-gbp-1hour*
;;   (make-trading-style
;;    *eur-gbp*
;;    "IDEALPRO"
;;    "1 hour"
;;    "3600 S"
;;    "3660 S"
;;    "1 Y"
;;    "4 W"
;;    ))

(define *trading-styles*
  (map (^p (fifteen-min-style p))
       (list (make-currency-pair "EUR" "GBP")
             (make-currency-pair "EUR" "USD")
             (make-currency-pair "EUR" "CHF")
             (make-currency-pair "GBP" "USD")
             (make-currency-pair "GBP" "CHF")
             (make-currency-pair "USD" "CHF")
             )))

;; (define *eur-gbp* (make-currency-pair "EUR" "GBP"))
;; (define *eur-usd* (make-currency-pair "EUR" "USD"))
;; (define *eur-chf* (make-currency-pair "EUR" "CHF"))
;; (define *gbp-usd* (make-currency-pair "GBP" "USD"))
;; (define *gbp-chf* (make-currency-pair "GBP" "CHF"))
;; (define *chf-usd* (make-currency-pair "CHF" "USD"))

;; (define *eur-gbp-15min* (fifteen-min-style *eur-gbp*))
;; (define *eur-usd-15min* (fifteen-min-style *eur-usd*))
;; (define *gbp-usd-15min* (fifteen-min-style *gbp-usd*))


(define *trading-style-table* (make-hash-table))

(define (query-history style)
  (let* ((date (latest-bar-closing-date (current-date) style))
         (last-data (query-data *conn* (currency-pair-name
                                        (trading-style-currency-pair style))
                                date 1 (trading-style-bar-size style)))
         (duration
          (if (zero? (data-set-count last-data))
              (trading-style-history-period style)
              (let ((sec
                     (time-second
                      (time-difference
                       (date->time-utc date)
                       (date->time-utc (bar-date (car (data-set-rows last-data))))))))
                (if (> sec 86400)
                    (trading-style-min-period style)
                    #`",sec S")))))
    (if (string=? duration "0 S")
        (sleep-and-update style)
        (update-history style duration)
        )))

(define (on-next-valid-id id)
  (set! *order-id* id)
  (enqueue! *task-queue* (^[] (for-each query-history *trading-styles*))))

(define (on-historical-data req-id time open high low close volume count wap)
  (let ((style (hash-table-get *trading-style-table* req-id))
        (date (string->date time "~Y~m~d  ~H:~M:~S"))) ; "20190830  22:00:00"
    (add-data *conn*
              (currency-pair-name (trading-style-currency-pair style))
              (trading-style-bar-size style)
              date open close high low)))

(define (on-order-status order-id status filled remaining avg-fill-price perm-id
                         parent-id last-fill-price client-id why-held mkt-cap-price)
  (debug-log "on-order-status" order-id status)
  (when (string=? status "Filled")
        (let ((callback (hash-table-get *order-status-callbacks* order-id #f)))
          (when callback
                (hash-table-put! *order-status-callbacks* order-id #f)
                (enqueue! *task-queue* (lambda () (callback avg-fill-price)))))))

(define *task-queue* (make-mtqueue))

(define (update-history style duration)
  (let* ((req-id (request-id!))
         (date (latest-bar-closing-date (current-date) style))
         (date-str (date->string date "~Y~m~d ~T")))
    (hash-table-put! *trading-style-table* req-id style)
    (tws-client-historical-data-request
     *tws* req-id
     (currency-pair-symbol (trading-style-currency-pair style))
     "CASH"
     (currency-pair-currency (trading-style-currency-pair style))
     "IDEALPRO" ;;(trading-style-exchange style)
     date-str
     duration
     (trading-style-bar-size style)
     "MIDPOINT")
    ))

(define (sleep-and-update style)
  (let ((min (date-minute (current-date))))
    (thread-start!
     (make-thread
      (lambda ()
        (let* ((sec (* 60 (let ((rest (modulo (- 60 min) 15)))
                            (if (zero? rest)
                                15
                                rest)))) ; 15 min
               (count (quotient sec 2)))
          (let loop ((count count))
            (if (zero? count)
                (enqueue! *task-queue*
                  (lambda ()
                    (update-history style (trading-style-duration-for-query style))))
                (begin
                  (sys-sleep 2)
                  (tws-client-request-current-time *tws*)
                  (loop (- count 1)))))))))))

(define (get-all-positions style)
  (let* ((key #`"positions:,(currency-pair-name (trading-style-currency-pair style))")
         (vec (redis-hgetall *conn* key)))
    (let loop ((i 0) (part ()))
      (if (< i (vector-length vec))
          (loop (+ i 2)
                (cons (cons (vector-ref vec i)
                            (deserialize-position (read-from-string (vector-ref vec (+ i 1)))))
                      part))
          part))))

(define (position-id)
  (redis-get *conn* "position-id"))

(define (position-id-bump!)
  (redis-incr *conn* "position-id"))

(define *quantitiy-unit* 10000.0)       ; minimum size = 20K

(define (set-profits pos-id close-order)
  (debug-log "Really Closing...")
  (let* ((dat (get-order-data *conn* pos-id))
         (sym (order-data-symbol dat))
         (cur (order-data-currency dat))
         (pos (get-position *conn* sym cur pos-id))
         (pos-key (entry-price-key (make-currency-pair sym cur))))
    (if (and pos dat)
        (order (case (position-action pos)
                 ((sell) "BUY")
                 ((buy) "SELL"))
               (order-data-symbol dat)
               (order-data-currency dat)
               (order-data-exchange dat)
               (order-data-quantity dat)
               (lambda (oid price)
                 (delete-position *conn* sym cur pos-id)
                 (let ((res (redis-hget *conn* pos-key pos-id)))
                   (if res
                       (let ((open-price (string->number res)))
                         (log-result *conn* pos-id oid dat (position-action pos) open-price price
                                     pos close-order (current-date)))
                       (debug-log #`"ERROR: Position not found: ,pos-id")))))
        (debug-log #`"Redis entry not found: ,pos-id"))))

(define (close-position pos-id close-order)
  (debug-log "Closing" close-order)
  ;; (close "95" 1.098735 loss -9.999999999998899e-5)
  ;;; (list 'close pos-idx price result gain)
  (match close-order
         (('close _ _ 'loss . _)
          (debug-log "Stop loss...")
          (set-profits pos-id close-order))    ; FIXME handle stop loss with Stop Limit
         (('close _ _ 'gain . _)
          (set-profits pos-id close-order))
         (('close _ _ 'manual . _)
          (set-profits pos-id close-order))))

;; positions : pos-id -> [position]
;; order-data : pos-id -> [order-id symbol currentcy exchange]

(define (entry-price-key cur-pair)
  #`"entry-price:,(currency-pair-name cur-pair)")

(define (open-position pos-id style pos)
  (debug-log "Opening" (serialize-position pos))

  (let* ((cur-pair (trading-style-currency-pair style))
         (sym (currency-pair-symbol cur-pair))
         (cur (currency-pair-currency cur-pair))
         (exc (trading-style-exchange style))
         (qty *quantitiy-unit*)
         (pos-key (entry-price-key cur-pair)))
  (order (case (position-action pos)
           ((sell) "SELL")
           ((buy) "BUY"))
         sym cur exc qty
         (lambda (oid price)
           (save-position *conn* sym cur pos-id pos (make-order-data oid sym cur exc qty))
           (redis-hset *conn* pos-key pos-id price)))))

(define (orders-key symbol currecy exchange)
  #`"orders:,|symbol|:,|currecy|:,|exchange|")

(define *order-status-callbacks* (make-hash-table))

(define (order action symbol currecy exchange quantity proc)
  (debug-log "ordering")
  (enqueue! *task-queue*
            (lambda ()
              (let ((oid (order-id!))
                    (contract (make-tws-contract symbol "CFD" currecy exchange))
                    (order (make-tws-order action "MKT" quantity 0 0 0)))
                (tws-client-place-order *tws* oid contract order)
                (log-order *conn* oid action symbol currecy exchange quantity)
                (hash-table-put! *order-status-callbacks* oid (lambda (price) (proc oid price)))))))

(define (on-historical-data-end req-id start-date end-date)
  (let ((style (hash-table-get *trading-style-table* req-id)))
    (let-values (((pos poss) (inspect *conn* style (current-date) (get-all-positions style)
                                      close-position)))
      (when pos
            (open-position (position-id) style pos)
            (position-id-bump!)))

    (sleep-and-update style)))

(define (on-current-time time)
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
      (tws-client-process-messages *tws*)
      (loop)))))
