;; parse-expression : SchemeVal -> LcExp
(define (parse-expression datum)
  (cond
    [(symbol? datum) (var-exp datum)]
    [(pair? datum)
     (if (eqv? (car datum) 'lambda)
         (lambda-exp
          (car (cadr datum))
          (parse-expression (caddr datum)))
         (app-exp
          (parse-expression (car datum))
          (parse-expression (cadr datum))))]
    [else (error "Invalid concrete-syntax")]))

;; unparse-lc-exp: LcExp -> SchemeVal

(define (unparse-lc-exp exp)
  (cases lc-exp exp
         (var-exp (var) var)
         (lambda-exp (bound-var body)
                     (list 'lambda
                           (list bound-var)
                           (unparse-lc-exp body)))
         (epp-exp (rator rand)
                  (list (unparse-lc-exp rator)
                        (unparse-lc-exp rand)))))
;; 2.29
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))
