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

(define *historical-data-handers* (make-hash-table))
(define *historical-data-end-handers* (make-hash-table))

(define (on-next-valid-id id)
  (set! *order-id* id)
  (enqueue! *task-queue*
            (^[]
              (let* ((duration "1 D")
                     (req-id (request-id!))
                     (date (current-date))
                     (date-str (date->string date "~Y~m~d ~T")))

                (hash-table-put!
                 *historical-data-handers* req-id
                 (lambda (time open high low close volume count wap)
                   (debug-log "[history data]" time open high low close)
                   ))

                (hash-table-put!
                 *historical-data-end-handers* req-id
                 (lambda (start-date end-date)
                   (debug-log "Historical data end" start-date end-date)))
                
                (tws-client-historical-data-request
                 *tws* req-id
                 "EUR"
                 "CASH"
                 "GBP"
                 "IDEALPRO" ;;(trading-style-exchange style)
                 date-str
                 duration
                 "4 hours"
                 "MIDPOINT")
                ))))

(define (on-historical-data req-id time open high low close volume count wap)
  (let ((handler (hash-table-get *historical-data-handers* req-id #f)))
    (when handler
      (enqueue! *task-queue*
                (lambda ()
                  (handler time open high low close volume count wap))))))

(define (on-order-status order-id status filled remaining avg-fill-price perm-id
                         parent-id last-fill-price client-id why-held mkt-cap-price)
  (debug-log "on-order-status" order-id status)
  (when (string=? status "Filled")
        (let ((callback (hash-table-get *order-status-callbacks* order-id #f)))
          (when callback
                (hash-table-put! *order-status-callbacks* order-id #f)
                (enqueue! *task-queue* (lambda () (callback avg-fill-price)))))))

(define *task-queue* (make-mtqueue))

(define (on-historical-data-end req-id start-date end-date)
  (let ((handler (hash-table-get *historical-data-end-handers* req-id #f)))
    (when handler
      (enqueue! *task-queue*
                (lambda ()
                  (handler start-date end-date))))))

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
