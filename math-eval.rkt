(define (math-eval expr)
  (cond [(number? expr) expr]
        [else (math-eval-combo expr)]))


(define (math-eval-combo expr)
  (let [(operator-name (car expr))
        (arg1 (math-eval (cadr expr)))
        (arg2 (math-eval (caddr expr)))]
    (cond
     [(eq? operator-name 'plus) (+ arg1 arg2)]
     [(eq? operator-name 'sub) (- arg1 arg2)]
     [(eq? operator-name 'mul) (* arg1 arg2)]
     [(eq? operator-name 'div) (/ arg1 arg2)]
     [else (error "Invalid expr")])))
