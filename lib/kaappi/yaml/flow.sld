(define-library (kaappi yaml flow)
  (import (scheme base) (scheme char))
  (export read-fmap read-fseq read-dq read-sq parse-scalar
          parse-inline str-trim-left str-trim-right str-prefix?
          yaml-null yaml-null?)
  (begin

    (define yaml-null 'null)
    (define (yaml-null? v) (eq? v 'null))

    (define (str-trim-left s)
      (let loop ((i 0))
        (if (or (= i (string-length s))
                (not (char=? (string-ref s i) #\space)))
            (substring s i (string-length s))
            (loop (+ i 1)))))

    (define (str-trim-right s)
      (let loop ((i (- (string-length s) 1)))
        (if (or (< i 0) (not (char=? (string-ref s i) #\space)))
            (substring s 0 (+ i 1))
            (loop (- i 1)))))

    (define (str-prefix? prefix s)
      (and (>= (string-length s) (string-length prefix))
           (string=? (substring s 0 (string-length prefix)) prefix)))

    (define (parse-scalar s)
      (cond
        ((string=? s "") yaml-null) ((string=? s "~") yaml-null)
        ((string=? s "null") yaml-null) ((string=? s "Null") yaml-null)
        ((string=? s "true") #t) ((string=? s "True") #t)
        ((string=? s "false") #f) ((string=? s "False") #f)
        ((string=? s ".inf") +inf.0) ((string=? s "-.inf") -inf.0)
        ((string=? s ".nan") +nan.0)
        ((string->number s) => (lambda (n) n))
        (else s)))

    (define (skip-fw port)
      (let loop ()
        (let ((ch (peek-char port)))
          (when (and (not (eof-object? ch))
                     (or (char=? ch #\space) (char=? ch #\newline)
                         (char=? ch #\return) (char=? ch #\tab)))
            (read-char port) (loop)))))

    (define (read-dq port)
      (read-char port)
      (let loop ((acc '()))
        (let ((ch (read-char port)))
          (cond
            ((or (eof-object? ch) (char=? ch #\")) (list->string (reverse acc)))
            ((char=? ch #\\)
             (let ((e (read-char port)))
               (loop (cons (cond ((char=? e #\n) #\newline) ((char=? e #\t) #\tab)
                                 ((char=? e #\\) #\\) ((char=? e #\") #\") (else e)) acc))))
            (else (loop (cons ch acc)))))))

    (define (read-sq port)
      (read-char port)
      (let loop ((acc '()))
        (let ((ch (read-char port)))
          (cond
            ((eof-object? ch) (list->string (reverse acc)))
            ((char=? ch #\')
             (if (and (not (eof-object? (peek-char port)))
                      (char=? (peek-char port) #\'))
                 (begin (read-char port) (loop (cons #\' acc)))
                 (list->string (reverse acc))))
            (else (loop (cons ch acc)))))))

    (define (read-flow port)
      (skip-fw port)
      (let ((ch (peek-char port)))
        (cond
          ((eof-object? ch) yaml-null)
          ((char=? ch #\{) (read-fmap port))
          ((char=? ch #\[) (read-fseq port))
          ((char=? ch #\") (read-dq port))
          ((char=? ch #\') (read-sq port))
          (else (read-fscalar port)))))

    (define (read-fmap port)
      (read-char port)
      (let loop ((acc '()))
        (skip-fw port)
        (let ((ch (peek-char port)))
          (cond
            ((or (eof-object? ch) (char=? ch #\})) (read-char port) (reverse acc))
            ((char=? ch #\,) (read-char port) (loop acc))
            (else
             (let ((key (read-fkey port)))
               (skip-fw port) (read-char port) (skip-fw port)
               (let ((val (read-flow port)))
                 (loop (cons (cons key val) acc)))))))))

    (define (read-fseq port)
      (read-char port)
      (let loop ((acc '()))
        (skip-fw port)
        (let ((ch (peek-char port)))
          (cond
            ((or (eof-object? ch) (char=? ch #\])) (read-char port) (reverse acc))
            ((char=? ch #\,) (read-char port) (loop acc))
            (else (loop (cons (read-flow port) acc)))))))

    (define (read-fkey port)
      (let ((ch (peek-char port)))
        (cond
          ((char=? ch #\") (read-dq port))
          ((char=? ch #\') (read-sq port))
          (else (let loop ((acc '()))
                  (let ((ch (peek-char port)))
                    (if (or (eof-object? ch) (char=? ch #\:))
                        (str-trim-right (list->string (reverse acc)))
                        (begin (read-char port) (loop (cons ch acc))))))))))

    (define (read-fscalar port)
      (let loop ((acc '()))
        (let ((ch (peek-char port)))
          (if (or (eof-object? ch) (char=? ch #\,)
                  (char=? ch #\}) (char=? ch #\]))
              (parse-scalar (str-trim-right (list->string (reverse acc))))
              (begin (read-char port) (loop (cons ch acc)))))))

    (define (parse-inline s)
      (let ((t (str-trim-left s)))
        (cond
          ((string=? t "") yaml-null)
          ((char=? (string-ref t 0) #\{) (read-fmap (open-input-string t)))
          ((char=? (string-ref t 0) #\[) (read-fseq (open-input-string t)))
          ((char=? (string-ref t 0) #\") (read-dq (open-input-string t)))
          ((char=? (string-ref t 0) #\') (read-sq (open-input-string t)))
          (else (parse-scalar t)))))))
