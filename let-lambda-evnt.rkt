;;eval can handle local binding

(define (eval expr evnt)
  (cond [(symbol? expr) (eval-symbol expr envt)]
        [(pair? expr) (eval-list expr envt)]
        [(self-evaluating? exprt) expr]
        [else (error "invalied expr")]))

;; bdg-type -> return a symbol saying what kind type binding it is.
;; bdg-variable-ref --> returns the value of a normal variable binding
;; bdg-special-form-evaluator --> returns an evaluation procedure for a special form binding
;; (define (eval-list list-expr evnt)
;;   (if (symbol? (car list-expr))
;;       (let
;;           [(binding-info (evnt-lexical-lookup evnt (car expr-list)))]
;;         (cond [(not binding-info) (error "unbinding symbol")]
;;               (else (cond [(eq? (binding-type binding-info)
;;                                 '<special-form>)
;;                            ((bdg-special-form-evaluator binding-info) list-expr evnt)]
;;                           [(eq? (binding-type  binding-info) '<variable>)
;;                            (eval-combo (bdg-variable-ref binding-info)
;;                                        (cdr list-expr) envt)]
;;                           [(eq? (binding-type binding-info) '<syntax>)
;;                            (eval-macro-call (bdg-syntax-transformer binding-info) list-expr envt)]

;;                           [else (error "Unrecognized binding type")]))))
;;       (eval-combo (eval (car list-expr) envt)
;;                   (cdr list-expr) envt)))

(define (eval-list expr envt)
  (if (symbol? (car expr))
      (let [(binding-info (envt-lexcial-lookup envt (car expr)))]
        (cond
         [(not binding-info) (error "Unbinding symbol")]
         [(eq? (binding-type binding-info) '<special-form>)
          ((bdg-special-form-evaluator binding-info) list-expr envt)]
         [(eq? (binding-type binding-info) '<variable>)
          (eval-combo (bdg-variable-ref binding-info) (car list-expr) envt)]
         [else (error "unrecognized binding type")]))
      (eval-combo (eval (car list-expr) envt) (cdr list-expr) envt)))

(define (eval-let let-form envt)
  (let [(binding-forms (cadr let-form))
        (body-forms (cddr (let-form)))]
    (let [(var-list (map car binding-forms))
          (init-expr-list (map cadr binding-forms))]
      (let [(new-envt (make-envt var-list (make-multi init-expr-list envt) envt))]
        (eval-sequence body-forms new-envt)))))

(define (eval-multi arg-forms envt)
  (map (lambda (x)
         (eval x envt)) arg-forms))

(define (eval-sequence arg-forms envt)
  (if (pair? arg-forms)
      (cond [(pair? (cdr arg-forms))
             (eval (car arg-forms) envt)
             (eval-sequence (cdr arg-forms) envt)]
            [else (eval (car arg-forms) envt)])
      '*undefined-value*))

(define (eval-symbol name-symbol envt)
  (let [(binding-info (envt-lexical-lookup envt name-symbol))]
    (cond [(not binding-info) (error "unbinding variable" name-symbol)]
          [(eq? (binding-type binding-info) '<variable>)
           (bdg-variable-ref binding-info)]
          [else (error "non-variable name reference")])))

(define (eval-set! set-form envt)
  (let [(name (cadr set-form))
        (value-expr (caddr set-form))]
    (let [(binding-info (envt-lexical-lookup envt name))]
      (cond [(not binding-info) (error ("unbound variable" name))]
            [(eq? (binding-type binding-info) '<variable>)
             (bdg-variable-set! binding-info (eval value-expr envt))]
            [else (error "non-variable" name)]))))
