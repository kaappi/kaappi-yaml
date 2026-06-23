(import (kaappi yaml))

(define config (yaml-read-string "
server:
  host: 0.0.0.0
  port: 8080
  debug: false

database:
  url: postgres://localhost/myapp
  pool: 5

routes:
  - /
  - /api
  - /health
"))

(display "Host: ") (display (yaml-ref* config "server" "host")) (newline)
(display "Port: ") (display (yaml-ref* config "server" "port")) (newline)
(display "DB: ") (display (yaml-ref* config "database" "url")) (newline)
(display "Routes: ") (display (yaml-ref config "routes")) (newline)
