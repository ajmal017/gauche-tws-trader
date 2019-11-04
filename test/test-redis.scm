(use gauche.test)

(add-load-path "./lib/")

(test-start "Redis")
(use redis)
(use config)

(define *conn* (redis-open redis-host redis-port))

(test "redis-hget" '("123" #f)
      (lambda ()
        (redis-del *conn* "test.hget")
        (redis-hset *conn* "test.hget" "a" 123)
        (list (redis-hget *conn* "test.hget" "a")
              (redis-hget *conn* "test.hget" "b"))))

(test "redis-zrange with scores" '("a" "100" "b" "200")
      (lambda ()
        (redis-zadd *conn* "test.zset" 100 "a")
        (redis-zadd *conn* "test.zset" 200 "b")
        (redis-zadd *conn* "test.zset" 300 "c")
        (let ((dat (redis-zrange *conn* "test.zset" 0 1 "withscores")))
          (vector->list dat))))
