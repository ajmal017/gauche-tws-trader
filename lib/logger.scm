(define-module logger
  (use srfi-19)
  (use redis)
  (use scheme.vector)
  (use scheme.comparator)

  (add-load-path "./lib/")
  (use trader)
  (use query)

  (export debug-log
          log-result
          log-order
          filter-log/currency-pair
          result-log->history-tree
          ))

(select-module logger)

(define (debug-log . rest)
  (let loop ((logs (cons (date->string (current-date) "~4") rest)))
    (when (pair? logs)
          (display (car logs) (current-error-port))
          (when (pair? (cdr logs))
                (display "\t" (current-error-port))
                (loop (cdr logs)))))
  (newline (current-error-port)))

(define (log-result conn pos-id order-id order-data action open-price close-price pos order date)
  (let* ((gain (case action
                 ((sell) (- open-price  close-price))
                 ((buy)  (- close-price open-price))))
         (sym (order-data-symbol   order-data))
         (cur (order-data-currency order-data))
         (qty (order-data-quantity order-data))
         (net-gain (* gain qty)))
    (debug-log #`"Closing order done: pos: ,pos-id order: ,order-id"
               #`"action: ,action"
               #`"open-price: ,open-price close-price: ,close-price gain: ,gain")
    (redis-zadd conn "result-log" pos-id #?=(write-to-string
                                             `((pos-id      . ,pos-id)
                                               (action      . ,action)
                                               (sym         . ,sym)
                                               (cur         . ,cur)
                                               (open-price  . ,open-price)
                                               (close-price . ,close-price)
                                               (gain        . ,gain)
                                               (net-gain    . ,net-gain)
                                               (position    . ,(serialize-position pos))
                                               (close-order . ,order)
                                               (date        . ,(date->string date "~4"))
                                               )))))

(define (log-order conn oid action symbol currecy exchange quantity)
  (redis-zadd conn "orderlog" oid
              (write-to-string (list oid (date->string (current-date) "~4")
                                     action symbol currecy exchange quantity))))

(define (filter-log/currency-pair conn sym cur num)
  (let ((logs (redis-zrevrange conn "result-log" 0 num)))
    (filter (lambda (log)
              (and (assoc 'position log)
                   (assoc 'close-order log)
                   (string=? sym (assoc-ref log 'sym))
                   (string=? cur (assoc-ref log 'cur))))
            (vector-fold-right (^[a b] (cons (read-from-string b) a)) '() logs))))

(define date-comparator
  (make-comparator
   date?
   (^[a b] (time=? (date->time-utc a) (date->time-utc b)))
   (^[a b] (time<? (date->time-utc a) (date->time-utc b)))
   #f
   'date-comparator))

(define (result-log->history-tree logs)
  (let* ((tree (make-tree-map date-comparator)))
    (for-each (lambda (log)
                (let* ((action (assoc-ref log 'action))
                       (pos (deserialize-position (assoc-ref log 'position)))
                       (open-date (position-date pos))
                       (open-price (assoc-ref log 'open-price))
                       (close (assoc-ref log 'close-order))
                       (close-date (string->date (assoc-ref log 'date) "~Y-~m-~dT~H:~M:~S~z"))
                       (close-price (assoc-ref log 'close-price))
                       (close-action (case action ((sell) 'buy) ((buy) 'sell))))
                  (tree-map-put! tree open-date `(,action ,open-price))
                  (tree-map-put! tree close-date `(,close-action ,close-price))))
              logs)
    tree))
