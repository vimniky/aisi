(define (read-token)
  (let (first-char (read-char))
    (cond
     [(char-whitespace? fisrt-char) (read-token)]
     [(eq? first-char #\() left-parent-token]
     [(eq? first-char #\)) right-parent-token]
     [(char-alphabetic? first-char) (read-identifier first-char)]
     [(char-numberic? first-char) (read-number first-char)]
     [else (error "illegal syntax")])))

(define (char-whitespace? first-char)
  (or [eq? (char #\space)]
      [eq? (char #\newline)]))

(define left-parent-token (list '*left-parenthesis-token*))
(define right-parent-token (list '*right-parenthesis-token*))

(define (left-parenthesis-token? something)
  [eq? something left-parenthesis-token])

(define (right-parenthesis-token? something)
  [eq? something right-parenthesis-token])

(define (read-identifier chr)
  (define (read-identifier-helper list-so-far)
    (let [(next-char (peek-char))]
      [if (or [char-alphabetic? next-char]
              [char-numberic? next-char])
          (read-identifier-helper (cons next-char list-so-far))
          (reverse list-so-far)])
    (string->symbol (list->string (read-identifier-helper (list chr))))))

(define (read-number chr)
  (define (read-number-helper list-so-far)
    (let [(next-char (peek-char))]
      (if [ char-numberic? next-char ]
          [ read-number-helper (cons next-char list-so-far)]
          [ reverse list-so-far ]))
    (string->symbol (list->string (read-number-helper (list chr))))))

(define (simple-reader)
  (let [(next-token (read-token))]
    (cond [(token-letfpar? next-token) (read-list '())]
          [else next-token])))

(define (read-list list-so-far)
  (let [(token (micro-read-token))]
    (cond [(token-rightpar? token) (reverse list-so-ar)]
          [(token-leftpar? token) (read-list (cons (read-list '() list-so-far)))]
          [else (read-list (cons token list-so-far))])))
