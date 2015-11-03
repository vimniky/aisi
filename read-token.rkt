(define (read-token)
  (let (first-char (read-char))
    (cond
     [(char-whitespace? firt-char) (read-token)]
     [(eq? first-char #\() left-parent-token]
     [(eq? first-char #\)) right-parent-token]
     [(char-alphabetic? first-char) (read-identifier first-char)]
     [(char-numberic? first-char) (read-number first-char)]
     [else (error "illegal syntax")])))

