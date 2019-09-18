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
          (let ((ser (position->string pos)))
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
              (serialized (position->string pos))
              (deserialized (deserialize-position serialized))
              (serialized-again (position->string deserialized)))
         (match serialized-again
                (('position 123
                            "1978-01-24T11:00:00+0900"
                            'sell 1.2345 1.2346
                            ('poly 1 1.5 3.5)
                            ('pos-info 0.005
                                       ('poly 1.3 2.5 3.6) 1.234
                                       ('poly 3.4 5.6 6.7) 3.456))
                 #t))))
