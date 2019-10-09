(define-module config
  (export redis-host redis-port tws-host tws-port tws-client-id)
)

(select-module config)

(define redis-host "redis")
(define redis-port 6379)
(define tws-host "host.docker.internal")
(define tws-port 7497)
(define tws-client-id 10)
