(define-module position
  (use redis)
  (use redis.async)

  (use trader)

  (export get-position
          get-order-data
          save-position
          delete-position
          ))

(select-module position)

(define (get-position redis-conn pos-id)
  (let ((pos-str (redis-hget redis-conn "positions" pos-id)))
    (deserialize-position (read-from-string pos-str))))

(define (get-order-data redis-conn pos-id)
  (let ((ser (redis-hget redis-conn "order-data" pos-id)))
    (deserialize-order-data (read-from-string ser))))

(define (save-position redis-conn pos ord)
  #?=(redis-hset redis-conn "positions" (position-index pos)
              (write-to-string (serialize-position pos)))
  #?=(redis-hset redis-conn "order-data" (position-index pos)
              (write-to-string (serialize-order-data ord))))

(define (delete-position redis-conn pos-id)
  #?=(redis-hdel redis-conn "positions" pos-id)
  #?=(redis-hdel redis-conn "order-data" pos-id))
