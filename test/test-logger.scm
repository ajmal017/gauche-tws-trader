(use gauche.test)
(use srfi-19)
(use util.match)
(use redis)

(add-load-path "./lib/")
(use config)
(use trader)

(test-start "Logger")
(load "./lib/logger")
(import logger)

(test-module 'logger)

(define *conn* (redis-open redis-host redis-port))

(test* "filter-log/currency-pair" #t
       (pair? (filter-log/currency-pair *conn* "EUR" "GBP" 100)))

(define *logs* (filter-log/currency-pair *conn* "EUR" "GBP" 100))

(test "log entries" #t
      (lambda ()
        (for-each (lambda (log)
                    (match log
                           ((('pos-id . num)
                             ('action . action)
                             ('sym . sym)
                             ('cur . cur)
                             ('open-price . open-price)
                             ('close-price . close-price)
                             ('gain . gain)
                             ('net-gain . net-gain)
                             ('position . pos)
                             ('close-order 'close close-index _ type . _)
                             ('date . date))
                            (match close-index
                                   ((? number?) #t)
                                   ((? string?) #t))
                            (match (position-index (deserialize-position pos))
                                   ((? number?) #t)
                                   ((? string?) #t)))))
                  *logs*)
        #t))

(test* "result-log->history-tree" #t
       (let* ((tree (result-log->history-tree *logs*))
              (result (tree-map->alist tree)))
         (match result
                ((((? date?) (or 'sell 'buy) (? number?)) ..1) #t))
         (match (tree-map-max tree)
                (((? date?) . _) #t))
         #t
         ))
