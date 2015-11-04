(define (eval expr)
  (cond
   [(symbol? expr) (eval-variable expr)]
   [(pair? expr) (eval-list expr)]
   [(self-evaluateing? expr)  expr]
   [else (error "Illegal expression")]))

(define (self-evaluating? expr)
  (or
   [number? expr]
   [boolean? expr]))

(define (eval-list expr)
  (if [and (symbol? (car expr))
           (special-form-name? (car expr))]
      [eval-special-form expr]
      [eval-combo]))

(define (special-form-name? expr)
  (member '(if define set!)))

(define (eval-special-from expr)
  (let [(name (car expr))]
    (cond
     [(eq? name 'define) (eval-define expr)]
     [(eq? name 'if) (eval-if expr)]
     [(eq? name 'set!) (eval-set! expr)])))

(define (eval-if expr)
  (let [(expr-length (length expr))]
    (if
     [eval (cadr expr)]
     [eval (caddr expr)]
     [if [= expr-lenght 4]
         [eval (cadddr expr)]
         #t])))

