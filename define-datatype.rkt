;; define data type
;; define-datatype declaration has the form
(define-datatype typename type-predicate-name
  {(variant-name {(field-name preducate)}*)}+)
;; cases syntax
(cases type-name expression
       {(variant-name ({field-name}*) consequent)}
       (else default))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;; occurs-free: Sym x Lc-exp -> Bool
(define (occurs-free? search-var exp)
  (cases lc-exp exp
         (var-exp (var) (eqv? var search-var))
         (lambda-exp (bound-var body)
                     (and (eqv? search-var bound-var)
                          (occurs-free? search-var body)))
         (app-exp (rator dand)
                  (or
                   (occurs-free? search-var rator)
                   (occurs-free? search-var rand)))))

;; S-list ::= ({S-exp}*)
;; S-exp  ::=Symbol | S-list
(define-datatype s-list s-list?
  (empty-s-list)
  (no-empty-s-list
   (first s-exp?)
   (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-exp
   (slist s-list?)))

(define-datatype s-list  s-list?
  (an-s-list
   (sexps (list-of exp?))))

(define (list-of pred)
  (lambda (val)
    (of (null? val)
        (and (pair? val)
             (pred (car cal))
             ((list-of pred) (cdr val))))))
