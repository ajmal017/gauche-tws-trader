(define-module appmain
  (export app-start! on-next-valid-id on-historical-data on-order-status
          on-historical-data-end on-current-time))

(select-module appmain)


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

(define *conn* #f)


;;;;;;;;;;;;;;

(define *tws* #f)

(define *request-id* #f)
(define (request-id!)
  (let ((id *request-id*))
    (inc! *request-id*)
    id))

(define *order-id* #f)
(define (order-id!)
  (let ((id *order-id*))
    (inc! *order-id*)
    id))

#;(define *trading-styles*
  (map (^p (fifteen-min-style p))
       (list (make-currency-pair "EUR" "GBP")
             (make-currency-pair "EUR" "USD")
             (make-currency-pair "EUR" "CHF")
             (make-currency-pair "GBP" "USD")
             (make-currency-pair "GBP" "CHF")
             (make-currency-pair "USD" "CHF")
             )))

(define *historical-data-handlers* (make-hash-table))
(define *historical-data-end-handlers* (make-hash-table))

(define (make-event-handler hash label)
  (define (new-proc . args)
    (enqueue! *task-queue*
              (lambda ()
                (let* ((queue (hash-table-get hash (car args)))
                       (task (dequeue! queue #f)))
                  (if task
                      (task (cons label (cdr args)))
                      (apply new-proc args))))))
  new-proc)

;; Event handlers
(define on-historical-data
  (make-event-handler *historical-data-handlers* 'data))
(define on-historical-data-end
  (make-event-handler *historical-data-end-handlers* 'end))

;;

(define (request-historical-data . args)
  (let ((req-id (request-id!)))
    (hash-table-put! *historical-data-handlers* req-id (make-mtqueue))
    (hash-table-put! *historical-data-end-handlers* req-id (make-mtqueue))

    (apply tws-client-historical-data-request *tws* req-id args)

    (lambda (yield)
      (call/cc (lambda (cont)
                 (enqueue! (hash-table-get *historical-data-handlers* req-id) cont)
                 (enqueue! (hash-table-get *historical-data-end-handlers* req-id) cont)
                 (yield)
                 )))))

;;; application

(define (do-stuff)
  (let* ((date (current-date))
         (date-str (date->string date "~Y~m~d ~T")))
    (call/cc
     (lambda (cont)
       (let ((handle (request-historical-data
                      "EUR" "CASH" "GBP" "IDEALPRO"
                      date-str "1 W" "4 hours" "MIDPOINT")))
         (let loop ((data (handle cont)))
           (if (eq? (car data) 'end)
               (debug-log "historical data end" data)
               (begin
                 (debug-log "historical data" data)
                 (loop (handle cont)))
               )))

       (let ((handle (request-historical-data
                      "EUR" "CASH" "GBP" "IDEALPRO"
                      date-str "1 D" "1 hour" "MIDPOINT")))
         (let loop ((data (handle cont)))
           (if (eq? (car data) 'end)
               (debug-log "1-hour historical data end" data)
               (begin
                 (debug-log "1-hour historical data" data)
                 (loop (handle cont)))
               )))))))

(define (on-next-valid-id id)
  (set! *order-id* id)
  (enqueue! *task-queue* do-stuff))

(define (on-order-status order-id status filled remaining avg-fill-price perm-id
                         parent-id last-fill-price client-id why-held mkt-cap-price)
  (debug-log "on-order-status" order-id status)
  (when (string=? status "Filled")
        (let ((callback (hash-table-get *order-status-callbacks* order-id #f)))
          (when callback
                (hash-table-put! *order-status-callbacks* order-id #f)
                (enqueue! *task-queue* (lambda () (callback avg-fill-price)))))))

(define *task-queue* (make-mtqueue))

(define (on-current-time time)
  (debug-log #"on-current-time ~time")
  )

(define (app-start!)
  (set! *conn* (redis-open redis-host redis-port))
  (set! *tws* (make-tws-client))
  (set! *request-id* 4000)
  (tws-client-connect *tws* tws-host tws-port tws-client-id)

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
        (loop))))))
