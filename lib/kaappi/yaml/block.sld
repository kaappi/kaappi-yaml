(define-library (kaappi yaml block)
  (import (scheme base) (scheme char) (kaappi yaml flow))
  (export parse-bseq-fn parse-rmap-fn)
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

    (define (split-kv content)
      (let ((cp (find-colon content)))
        (if (not cp) (cons content "")
            (cons (str-trim-right (substring content 0 cp))
                  (if (< (+ cp 1) (string-length content))
                      (str-trim-left (substring content (+ cp 1) (string-length content)))
                      "")))))

    (define (parse-rmap-fn parse-val-fn lines si)
      (let loop ((ls lines) (acc '()))
        (let ((ls2 (skip-blank ls)))
          (cond
            ((null? ls2) (cons (reverse acc) '()))
            ((not (= (line-indent (car ls2)) si)) (cons (reverse acc) ls2))
            ((not (has-colon? (line-content (car ls2)))) (cons (reverse acc) ls2))
            (else
             (let* ((kv (split-kv (line-content (car ls2)))) (k (car kv)) (v (cdr kv)))
               (if (string=? v "")
                   (let* ((vr (parse-val-fn (cdr ls2) (+ si 1)))
                          (val (car vr)) (rest (cdr vr)))
                     (loop rest (cons (cons k val) acc)))
                   (loop (cdr ls2) (cons (cons k (parse-inline v)) acc)))))))))

    (define (do-seq-map-val parse-val-fn item rest-lines bi)
      (let* ((kv (split-kv item)) (k (car kv)) (v (cdr kv)))
        (if (string=? v "")
            (let* ((vr (parse-val-fn rest-lines (+ bi 3))))
              (let* ((mr (parse-rmap-fn parse-val-fn (cdr vr) (+ bi 2))))
                (cons (cons (cons k (car vr)) (car mr)) (cdr mr))))
            (let* ((mr (parse-rmap-fn parse-val-fn rest-lines (+ bi 2))))
              (cons (cons (cons k (parse-inline v)) (car mr)) (cdr mr))))))

    (define (parse-bseq-fn parse-val-fn lines bi)
      (let loop ((ls lines) (acc '()))
        (let ((ls2 (skip-blank ls)))
          (cond
            ((null? ls2) (cons (reverse acc) '()))
            ((not (= (line-indent (car ls2)) bi)) (cons (reverse acc) ls2))
            ((not (str-prefix? "- " (line-content (car ls2))))
             (cons (reverse acc) ls2))
            (else
             (let* ((cnt (line-content (car ls2)))
                    (item (substring cnt 2 (string-length cnt)))
                    (trimmed (str-trim-left item)))
               (cond
                 ((string=? trimmed "")
                  (let* ((vr (parse-val-fn (cdr ls2) (+ bi 1))))
                    (loop (cdr vr) (cons (car vr) acc))))
                 ((has-colon? item)
                  (let ((pr (do-seq-map-val parse-val-fn item (cdr ls2) bi)))
                    (loop (cdr pr) (cons (car pr) acc))))
                 (else
                  (loop (cdr ls2)
                        (cons (parse-inline (strip-comment item)) acc))))))))))))

