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
