(define-library (kaappi yaml parse)
  (import (scheme base) (scheme char)
          (kaappi yaml flow)
          (kaappi yaml block))
  (export yaml-parse-string)
  (begin

    (define (blank-or-comment? line)
      (let ((c (str-trim-left line)))
        (or (string=? c "")
            (and (> (string-length c) 0) (char=? (string-ref c 0) #\#)))))

    (define (line-indent line)
      (let loop ((i 0))
        (if (or (= i (string-length line))
                (not (char=? (string-ref line i) #\space)))
            i (loop (+ i 1)))))

    (define (line-content line)
      (substring line (line-indent line) (string-length line)))

    (define (skip-blank lines)
      (cond ((null? lines) '())
            ((blank-or-comment? (car lines)) (skip-blank (cdr lines)))
            (else lines)))

    (define (strip-comment s)
      (let loop ((i 0))
        (cond
          ((= i (string-length s)) (str-trim-right s))
          ((and (char=? (string-ref s i) #\#) (> i 0)
                (char=? (string-ref s (- i 1)) #\space))
           (str-trim-right (substring s 0 (- i 1))))
          (else (loop (+ i 1))))))

    (define (find-colon content)
      (let loop ((i 0))
        (cond
          ((= i (string-length content)) #f)
          ((char=? (string-ref content i) #\:)
           (if (or (= (+ i 1) (string-length content))
                   (char=? (string-ref content (+ i 1)) #\space))
               i (loop (+ i 1))))
          (else (loop (+ i 1))))))

    (define (has-colon? c) (if (find-colon c) #t #f))

    (define (string->lines str)
      (let loop ((i 0) (start 0) (acc '()))
        (cond
          ((= i (string-length str))
           (reverse (if (> i start) (cons (substring str start i) acc) acc)))
          ((char=? (string-ref str i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
          (else (loop (+ i 1) start acc)))))

    (define (split-kv content)
      (let ((cp (find-colon content)))
        (if (not cp) (cons content "")
            (cons (str-trim-right (substring content 0 cp))
                  (strip-comment
                   (if (< (+ cp 1) (string-length content))
                       (str-trim-left (substring content (+ cp 1) (string-length content)))
                       ""))))))

    (define (parse-bmap lines bi)
      (let loop ((ls lines) (acc '()))
        (let ((ls2 (skip-blank ls)))
          (cond
            ((null? ls2) (cons (reverse acc) '()))
            ((not (= (line-indent (car ls2)) bi)) (cons (reverse acc) ls2))
            ((not (has-colon? (line-content (car ls2)))) (cons (reverse acc) ls2))
            (else
             (let* ((kv (split-kv (line-content (car ls2))))
                    (k (car kv)) (v (cdr kv)))
               (if (string=? v "")
                   (let* ((vr (parse-val (cdr ls2) (+ bi 1)))
                          (val (car vr)) (rest (cdr vr)))
                     (loop rest (cons (cons k val) acc)))
                   (loop (cdr ls2) (cons (cons k (parse-inline v)) acc)))))))))

    (define (parse-val lines mi)
      (let ((ls (skip-blank lines)))
        (cond
          ((null? ls) (cons yaml-null '()))
          ((< (line-indent (car ls)) mi) (cons yaml-null ls))
          ((str-prefix? "- " (line-content (car ls)))
           (parse-bseq-fn parse-val ls (line-indent (car ls))))
          ((or (str-prefix? "{" (line-content (car ls)))
               (str-prefix? "[" (line-content (car ls))))
           (cons (parse-inline (line-content (car ls))) (cdr ls)))
          ((has-colon? (line-content (car ls)))
           (parse-bmap ls (line-indent (car ls))))
          (else
           (cons (parse-inline (strip-comment (line-content (car ls))))
                 (cdr ls))))))

    (define (yaml-parse-string str)
      (car (parse-val (string->lines str) 0)))))
