;; t-l-event for holding interpter's environment and initializing
;; and empty list
(define t-l-envt '())

;; for adding bindings
(define (toplevel-bind! name value)
  (let [(bdg (assoc name t-l-envt))] ;searching for association name
    (if bdg
        [set! (car bdg) value]
        [set! t-l-envt (cons (list name value) t-lenvt)])))

;; define accessor
(define (toplevel-get name)
  (assoc name t-l-envt))

(define (toplevel-set! name value)
  (set-car! (cdr (assoc name t-l-envt)) value))

(define (eval-variable symbol)
  (toplevel-get symbol))

(define (eval-define! expr)
  (toplevel-bind! (cadr expr)
                  (eval (caddr expr))))

(define (eval-set! expr)
  (toplevel-set! (cadr expr)
                 (eval (caddr expr))))
