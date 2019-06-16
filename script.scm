(use gauche.threads)
(use gauche.collection)
(use rfc.http)
(use sxml.tools)

(use srfi-19)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)

(add-load-path "./lib/")
(use violet)

(use config)

(use dbi)
(use dbd.pg)

;;
;; Application
;;

(define *conn* (dbi-connect #`"dbi:pg:user=postgres;host=,db-host"))

(define (query-data conn end-time count size)
  (let ((query (dbi-prepare conn "SELECT DISTINCT time, open, high, low, close FROM bars WHERE time <= to_timestamp(?) and size = ? order by time desc limit ?")))
    (let* ((end-sec (time->seconds end-time))
           (result (dbi-execute query end-sec size count))
           (getter (relation-accessor result))
           (dest (fold (lambda (row part)
                         (let ((highest (car part))
                               (lowest (cadr part))
                               (rows (caddr part))
                               (count (cadddr part))
                               (high (string->number (getter row "high")))
                               (low (string->number (getter row "low"))))
                           (list
                            (max high highest)
                            (min low lowest)
                            (cons (list (date->time-utc (string->date (getter row "time")
                                                                      "~Y-~m-~d ~H:~M:~S"))
                                        (string->number (getter row "open"))
                                        (string->number (getter row "close"))
                                        high
                                        low)
                                  rows)
                            (+ 1 count))))
                       '(0 9999999 () 0) result)))
      dest)))

(define (low-of row) (car (cddddr row)))
(define (high-of row) (cadddr row))

(define (make-bar-from-row row chart-width half-bar-width bar-width count index transform-y)
  (let ((time  (car row))
        (open  (cadr row))
        (close (caddr row))
        (high  (cadddr row))
        (low   (car (cddddr row))))
    (let* ((date (time-utc->date time))
           (x (x->integer (* chart-width (/ index count))))
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

(define (make-line-poly x0 y0 x1 y1)
  ;; returns polynomial in x + by + c = 0
  (let* ((b (- (/ (- x0 x1) (- y0 y1))))
         (c (- (+ x0 (* b y0)))))
    (cons b c)))

(define (distance-to-line x y poly)
  (let ((b (car poly))
        (c (cdr poly)))
    (+ y (/ (+ x c) b))))

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
        line
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
                        (if (> min-total-distance total-distance)
                            (loop2 (kdr rest) line-poly     total-distance     (+ x1 step))
                            (loop2 (kdr rest) min-line-poly min-total-distance (+ x1 step)))
                        (let* ((distance (distance-to-line count (pick (car rows)) line-poly)))
                          (if (or (= count x0) (= count x1))
                              (loop3 (kdr rows) total-distance (+ count step))
                              (if (filter distance)
                                  (loop2 (kdr rest) min-line-poly min-total-distance (+ x1 step))
                                  (loop3 (kdr rows) (accum total-distance distance) (+ count step)))
                              )))))))))))

(define (splice-data data offset length)
  (let ((list (rows-of data))
        (total-length (count-of data)))
    (take (take-right list (- total-length offset)) length)))

(define rows-of caddr)
(define count-of cadddr)

(define (min-line/range data offset points)
  (offset-line (line-from-rows (splice-data data offset points) low-of negative? + 1) offset))

(define (max-line/range data offset points)
  (offset-line (line-from-rows (splice-data data offset points) high-of positive? - 1) offset))

(define (min-line/range/step data offset points step)
  (offset-line (line-from-rows (splice-data data offset points) low-of negative? + step) offset))

(define (max-line/range/step data offset points step)
  (offset-line (line-from-rows (splice-data data offset points) high-of positive? - step) offset))

(define (offset-line poly offset-x)
  (cons (car poly) (- (cdr poly) offset-x)))

(define (draw-line poly chart-width count transform half-bar-width)
  (let ((b (car poly))
        (c (cdr poly))
        (x0 0)
        (x1 (- count 1)))
    `(line (@ (x1 ,(+ (x->integer (* chart-width (/ x0 count))) half-bar-width))
              (y1 ,(transform (- (/ (+ x0 c) b))))
              (x2 ,(+ (x->integer (* chart-width (/ x1 count))) half-bar-width))
              (y2 ,(transform (- (/ (+ x1 c) b))))
              (style ,#`"stroke:black;stroke-width:1"))
           )))

(define (format-data data end-time)
  (let ((chart-height 500)
        (chart-width 1000))
    `(,(let ((highest (car data))
             (lowest (cadr data))
             (rows (caddr data))
             (count (cadddr data)))
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
                  (draw-line* (^[poly] (draw-line poly chart-width count transform-y half-bar-width))))
             `(svg (@ (width ,chart-width) (height ,chart-height))
                   ,@(let loop ((rows rows)
                                (index 0)
                                (dest ()))
                       (if (null? rows)
                           (reverse dest)
                           (let ((row (car rows)))
                             (let ((bar (make-bar-from-row row chart-width half-bar-width bar-width
                                                           count index transform-y)))
                               (loop (cdr rows) (+ 1 index) (cons bar dest))))))
                   ,(draw-line* (min-line/range/step data 0 (- count 48) 4))
                   ,(draw-line* (min-line/range data (- count 48) 24))
                   ,(draw-line* (max-line/range/step data 0 (- count 48) 4))
                   ,(draw-line* (max-line/range data (- count 48) 24))
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


(define-http-handler #/^\/(\d+)\/(\d+)\/(\d+)\/(\d+)\/(\d+)\/?/
  (^[req app]
    (let-params req ([year   "p:1" :convert x->integer]
                     [month  "p:2" :convert x->integer]
                     [date   "p:3" :convert x->integer]
                     [hour   "p:4" :convert x->integer]
                     [minute "p:5" :convert x->integer])
      (violet-async
       (^[await]
         (let* ((end-time (date->time-utc (make-date 0 0 minute hour date month year 0)))
                (data (await (^[] (query-data *conn* end-time (* 24 5 4) "1 hour")))))
           (respond/ok req (cons "<!DOCTYPE html>"
                                 (sxml:sxml->html
                                  (create-page
                                   `(html (body (p ,#`"USD.EUR ,(date->string (time-utc->date end-time))")
                                                (h2 "1 hour")
                                                (div ,@(format-data data end-time))
                                                ))))))))))))

(define-http-handler "/"
  (^[req app]
    (respond/redirect req "/2019/1/1/0/0") ))

(define-http-handler #/^\/static\// (file-handler))
