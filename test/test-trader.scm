(use gauche.test)

(use srfi-19)

(use util.match)


(test-start "Trader")
(load "./lib/trader")
(import trader)

(test-module 'trader)


(test-section "match")

(test* "predicate" 123
       (match 123
              ((? number? x) x)))

(test* "predicate in list" 123
       (match '(aho 123 ('hoge 456))
              ((head
                (? number? x)
                (head2 y))
               x)))

(test* "..." '(1 2 3)
       (match '(aho 1 2 3)
              ((x rest ...)
               rest)))

(test-section "position")

(test* "make-position" #t
        (let ((pos (make-position 123
                                  (make-date 0 0 0 11 24 1 1978 (* 9 3600))
                                  'sell
                                  1.2345
                                  1.2346
                                  (make-poly 1 1.5 3.5)
                                  (make-pos-info 0.005
                                                 (make-poly 1.3 2.5 3.6)
                                                 1.234
                                                 (make-poly 3.4 5.6 6.7)
                                                 3.456)
                                  )))
          (let ((ser (serialize-position pos)))
            (match ser
                   (('position 123
                               "1978-01-24T11:00:00+0900"
                               'sell 1.2345 1.2346
                               ('poly 1 1.5 3.5)
                               ('pos-info 0.005
                                          ('poly 1.3 2.5 3.6) 1.234
                                          ('poly 3.4 5.6 6.7) 3.456))
                    #t)))))

(test* "deserialize-position" #t
       (let* ((pos (make-position 123
                                  (make-date 0 0 0 11 24 1 1978 (* 9 3600))
                                  'sell
                                  1.2345
                                  1.2346
                                  (make-poly 1 1.5 3.5)
                                  (make-pos-info 0.005
                                                 (make-poly 1.3 2.5 3.6)
                                                 1.234
                                                 (make-poly 3.4 5.6 6.7)
                                                 3.456)
                                  ))
              (serialized (serialize-position pos))
              (deserialized (deserialize-position serialized))
              (serialized-again (serialize-position deserialized)))
         (match serialized-again
                (('position 123
                            "1978-01-24T11:00:00+0900"
                            'sell 1.2345 1.2346
                            ('poly 1 1.5 3.5)
                            ('pos-info 0.005
                                       ('poly 1.3 2.5 3.6) 1.234
                                       ('poly 3.4 5.6 6.7) 3.456))
                 #t))))

(test* "serialize-order-data" '(order-data 123 "EUR" "GBP" "IDEALPRO" 12345)
       (serialize-order-data (make-order-data 123 "EUR" "GBP" "IDEALPRO" 12345)))

(test* "deserialize-order-data" '(123 "EUR" "GBP" "IDEALPRO" 12345)
       (let ((dat (deserialize-order-data
                   (serialize-order-data (make-order-data 123 "EUR" "GBP" "IDEALPRO" 12345)))))
       (map (lambda (prod) (prod dat))
            (list order-data-order-id
                  order-data-symbol order-data-currency order-data-exchange
                  order-data-quantity))))

(test* "latest-bar-closing-date" "2019-10-07T15:30:00Z"
       (let* ((cur-date (make-date 0 0 35 15 7 10 2019 0))
              (result (latest-bar-closing-date cur-date (make-time time-duration 0 (* 60 15)))))
         (date->string result "~4")))

(test* "latest-bar-closing-date" "2019-10-07T15:45:00Z"
       (let* ((cur-date (make-date 0 0 45 15 7 10 2019 0))
              (result (latest-bar-closing-date cur-date (make-time time-duration 0 (* 60 15)))))
         (date->string result "~4")))

(test* "latest-bar-closing-date" "2019-10-07T15:00:00Z"
       (let* ((cur-date (make-date 0 0 0 15 7 10 2019 0))
              (result (latest-bar-closing-date cur-date (make-time time-duration 0 (* 60 15)))))
         (date->string result "~4")))

(test* "latest-bar-closing-date" "2019-10-07T15:00:00Z"
       (let* ((cur-date (make-date 0 0 20 15 7 10 2019 0))
              (result (latest-bar-closing-date cur-date (make-time time-duration 0 (* 60 60)))))
         (date->string result "~4")))

(test* "latest-bar-closing-date" "2019-10-07T15:00:00+0200"
       (let* ((cur-date (make-date 0 0 20 15 7 10 2019 (* 2 60 60)))
              (result (latest-bar-closing-date cur-date (make-time time-duration 0 (* 60 60)))))
         (date->string result "~4")))
