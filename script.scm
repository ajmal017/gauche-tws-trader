(use gauche.threads)
(use rfc.http)

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

(define (make-circle)
  '(svg (@ (width "100") (height "100"))
        (circle (@ (cx "50") (cy "50") (r "40")
                   (stroke "green") (stroke-width "4") (fill "yellow")))))

(define *conn* (dbi-connect "dbi:pg:postgres;host=britney.local" :username "postgres"))

(define (query-data)
  
)

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (respond/ok req `(sxml (html (body (h1 "SVG!")
                                          ,(make-circle)
                                          ))))))))

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

