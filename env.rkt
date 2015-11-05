;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;; Var Syc

;; empty-env : () -> Env
(define (empty-env)
    (list 'empty-env))

;; extend-env : Val x SchemeVal x Env -> Env
(define (extend-env var val env)
  (list 'extend-env var val env))

;; apply-env: Var x Env -> SchemeVal
(define (apply-env search-var env)
  (cond
    [(eqv? (car env) 'empty-env)
     (error "Value not found")]
    [(eqv? (car env) 'extend-env)
     (let ([saved-var (cadr env)]
           [saved-val (caddr env)]
           [saved-env (cadddr env)])
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env search-var saved-env)))]
    [else (error "Invalid env")]))

;; 2.5
(define empty-env '())

(define(empty-env? env)
  (eqv? env '())

(define (extend-env var val env)
  (cons (list var val)
        env))

(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars)
                   (cdr vals)
                   (extend-env (car vars)
                               (car vals)
                               env))))

(define (has-binding? search-var env)
  (if (empty-env? env)
      #f
      (let ([saved-var (caar env)]
            [saved-val (cadar env)]
            [saved-env (cdr env)])
        (if (eqv? search-var saved-var)
            #t
            (has-binding? search-var saved-env)))))

(define (apply-env search-var env)
  (if (empty-env? env)
      (error "Value not found")
      (let ([saved-var (caar env)]
            [saved-val (cadar env)]
            [saved-env (cdr env)])
        (cond
          [(eqv? search-var saved-var)
           saved-val]
          [else
           (apply-env search-var saved-env)]))))

;; Procedure representation
;; empty-env: () -> Env: var -> SchemeVal

(define (empty-env)
  (lambda (search-var)
    (error "Value not found")))

(define (extend-env saved-var saved-val saved-env)
  (lambda (search-var)
    (if (eqv? search-var saved-var)
        saved-val
        (saved-env search-var))))

(define (applay-env env search-var)
  (env search-var))
