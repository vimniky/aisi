;; eval with environmentt

(define (eval expr evnt)
  (cond
   [(symbol? expr) (eval-symbol expr evnt)]
   [(pair? expr) (eval-list expr evnt)]
   [(self-evaluating? expr) expr]
   [else (error "Invaliled expression")]))

(define (eval-list list-expr evnt)
  (if (symbol? (car list-expr))
      (let [(binding-info evnt-look-up evnt (car list-expr))]
        (cond
         [(not binding-info)
               (error "unbinding symbol" (car list-expr))]
         [(eq? (binding-type binding-info) '<special-form>)
          ((bdg-special-form-evaluator binding-info) list-expr evnt)]
         [(eq? (binding-type binding-info) '<variable>)
          (eval-combo (bdg-variable-ref binding-info)
                      (cdr list-expr) envt)]
         [(eq? (binding-type binding-info) '<syntax>)
          (eval-macro-call (bdg-syntax-transformer binding-info)
                           list-expr envt)]
         [else (error "Unrecognized binding type")]))))

(eval-combo (eval (car list-expr) envt)
            (cdr list-expr) envt)


(define (eval-let let-form envt)
  (let
      [(binding-forms (car let-form))]
    [let
        [(var-list (map car binding-forms))]
      [let ((new-envt (make-envt var-list
                                 (eval-multi init-expr-list envt)))
            eval-sequence body-forms new-envt)]]))
