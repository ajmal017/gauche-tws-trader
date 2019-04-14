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
      (link (@ (rel "stylesheet") (href "static/starter-template.css"))))
     (body
      (nav (@ (class "navbar navbar-expand-md navbar-dark bg-dark fixed-top"))
             (a (@ (href "#") (class "navbar-brand")) "Navbar")
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
       (div (@ (class "starter-template"))
              (h1 "Bootstrap starter template")
              (p
               (@ (class "lead"))
               "Use this document as a way to quickly start any new project."
               (br)
               " All you get is this text and a mostly barebones HTML document."))
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


(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (let* ((begin-time (date->time-utc (make-date 0 0 0 0 31 12 2018 0)))
              (end-time (date->time-utc (make-date 0 0 0 0 1 1 2019 0)))
              (data (await (^[] (query-data begin-time end-time)))))
         (respond/ok req (cons "<!DOCTYPE html>"
                               (sxml:sxml->html
                                (create-page
                                 `(html (body (h1 ,#`"USD.EUR ,(date->string (time-utc->date begin-time))")
                                              ,@(format-data data begin-time end-time)
                                              )))))))))))

(define-http-handler #/^\/static\// (file-handler))

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

