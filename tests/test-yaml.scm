(import (scheme base) (scheme write)
        (kaappi yaml))

(define pass 0)
(define fail 0)

(define-syntax check
  (syntax-rules (=>)
    ((_ expr => expected)
     (let ((result expr) (exp expected))
       (if (equal? result exp)
           (set! pass (+ pass 1))
           (begin
             (set! fail (+ fail 1))
             (display "FAIL: ") (write 'expr)
             (display " => ") (write result)
             (display ", expected ") (write exp)
             (newline)))))))

(define-syntax check-true
  (syntax-rules ()
    ((_ expr)
     (if expr
         (set! pass (+ pass 1))
         (begin
           (set! fail (+ fail 1))
           (display "FAIL: ") (write 'expr)
           (display " is false\n"))))))

;; --- Scalars ---

(display "Scalars\n")

(check (yaml-read-string "hello") => "hello")
(check (yaml-read-string "42") => 42)
(check (yaml-read-string "3.14") => 3.14)
(check (yaml-read-string "true") => #t)
(check (yaml-read-string "false") => #f)
(check (yaml-read-string "null") => 'null)
(check (yaml-read-string "~") => 'null)
(check (yaml-null? (yaml-read-string "null")) => #t)

;; --- Quoted strings ---

(display "Quoted strings\n")

(check (yaml-read-string "\"hello world\"") => "hello world")
(check (yaml-read-string "'hello world'") => "hello world")
(check (yaml-read-string "\"line1\\nline2\"") => "line1\nline2")
(check (yaml-read-string "'it''s'") => "it's")

;; --- Flow sequences ---

(display "Flow sequences\n")

(check (yaml-read-string "[1, 2, 3]") => '(1 2 3))
(check (yaml-read-string "[\"a\", \"b\"]") => '("a" "b"))
(check (yaml-read-string "[]") => '())
(check (yaml-read-string "[true, false, null]") => (list #t #f 'null))

;; --- Flow mappings ---

(display "Flow mappings\n")

(check (yaml-read-string "{a: 1, b: 2}") => '(("a" . 1) ("b" . 2)))
(check (yaml-read-string "{name: \"Alice\", age: 30}")
  => '(("name" . "Alice") ("age" . 30)))

;; --- Block mapping ---

(display "Block mapping\n")

(let ((r (yaml-read-string "name: Alice\nage: 30")))
  (check (yaml-ref r "name") => "Alice")
  (check (yaml-ref r "age") => 30))

(let ((r (yaml-read-string "host: localhost\nport: 8080\ndebug: true")))
  (check (yaml-ref r "host") => "localhost")
  (check (yaml-ref r "port") => 8080)
  (check (yaml-ref r "debug") => #t))

;; --- Block sequence ---

(display "Block sequence\n")

(check (yaml-read-string "- apple\n- banana\n- cherry")
  => '("apple" "banana" "cherry"))

(check (yaml-read-string "- 1\n- 2\n- 3")
  => '(1 2 3))

;; --- Nested mapping ---

(display "Nested structures\n")

(define nested-r (yaml-read-string "server:\n  host: localhost\n  port: 8080"))
(check (yaml-ref* nested-r "server" "host") => "localhost")
(check (yaml-ref* nested-r "server" "port") => 8080)

;; --- Mapping with sequence value ---

(display "Mapping with sequence\n")

(let ((r (yaml-read-string "colors:\n  - red\n  - green\n  - blue")))
  (check (yaml-ref r "colors") => '("red" "green" "blue")))

;; --- Comments ---

(display "Comments\n")

(let ((r (yaml-read-string "# header comment\nkey: value # inline")))
  (check (yaml-ref r "key") => "value"))

;; --- Special floats ---

(display "Special floats\n")

(check-true (infinite? (yaml-read-string ".inf")))
(check-true (nan? (yaml-read-string ".nan")))

;; --- yaml-ref ---

(display "yaml-ref*\n")

(check (yaml-ref* '(("a" . (("b" . (("c" . 42)))))) "a" "b" "c") => 42)
(check (yaml-ref* '(("x" . 1)) "nonexistent") => #f)

;; --- Writer ---

(display "Writer\n")

(check (yaml-write-string 42) => "42\n")
(check (yaml-write-string "hello") => "hello\n")
(check (yaml-write-string #t) => "true\n")
(check (yaml-write-string #f) => "false\n")
(check (yaml-write-string 'null) => "null\n")

;; --- Summary ---

(newline)
(display pass) (display " passed, ")
(display fail) (display " failed\n")
(when (> fail 0) (exit 1))
