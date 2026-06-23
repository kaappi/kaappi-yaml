# kaappi-yaml

YAML parser and writer for [Kaappi Scheme](https://github.com/kaappi/kaappi).

Pure Scheme — no C dependencies, no build step.

## Install

```bash
thottam install kaappi-yaml
```

## Quick start

```scheme
(import (kaappi yaml))

(define config (yaml-read-string "
server:
  host: localhost
  port: 8080
database:
  url: postgres://localhost/app
"))

(yaml-ref* config "server" "port")    ;=> 8080
(yaml-ref* config "database" "url")   ;=> "postgres://localhost/app"
```

## API

### Reading

```scheme
(yaml-read [port])           ; parse YAML from port
(yaml-read-string string)    ; parse YAML from string
```

### Writing

```scheme
(yaml-write value [port])    ; serialize to port
(yaml-write-string value)    ; serialize to string
```

### Access helpers

```scheme
(yaml-ref table key)          ; lookup key, returns #f if missing
(yaml-ref* table key ...)     ; nested lookup
(yaml-null? value)            ; check if value is YAML null
```

## Type mapping

| YAML type | Scheme type |
|-----------|-------------|
| Mapping | alist `(("key" . value) ...)` |
| Sequence | list |
| String | string |
| Integer | exact integer |
| Float | inexact number |
| Boolean | `#t` / `#f` |
| Null | `'null` symbol |
| .inf / .nan | `+inf.0` / `+nan.0` |

## Supported features

- Block mappings and sequences (indentation-based)
- Flow style (`{key: value}`, `[1, 2, 3]`)
- Double-quoted strings with escape sequences
- Single-quoted strings with `''` escaping
- Comments (`#` inline and full-line)
- Nested structures (mappings in sequences, sequences in mappings)
- Scalar type detection (integers, floats, booleans, null, strings)
- Special values (`.inf`, `-.inf`, `.nan`, `~`, `null`)

## License

MIT
