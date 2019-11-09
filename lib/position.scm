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

(define (get-position redis-conn symbol currency pos-id)
  (let* ((key #`"positions:,|symbol|.,|currency|")
         (pos-str (redis-hget redis-conn key pos-id)))
    (deserialize-position (read-from-string pos-str))))

(define (get-order-data redis-conn pos-id)
  (let* ((ser (redis-hget redis-conn "order-data" pos-id)))
    (deserialize-order-data (read-from-string ser))))

(define (save-position redis-conn symbol currency pos-id pos ord)
  (let ((pos-key #`"positions:,|symbol|.,|currency|"))
    (redis-hset redis-conn pos-key pos-id
                (write-to-string (serialize-position pos)))
    (redis-hset redis-conn "order-data" pos-id
                (write-to-string (serialize-order-data ord)))))

(define (delete-position redis-conn symbol currency pos-id)
  (let ((pos-key #`"positions:,|symbol|.,|currency|"))
    (redis-hdel redis-conn pos-key pos-id)
    (redis-hdel redis-conn "order-data" pos-id)))
