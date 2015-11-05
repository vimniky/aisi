;; interface for recursive data
;; Lc-exp ::= Identify
;;        ::= (lamda (Identify) Lc-exp)
;;        ::= (Lc-exp Lc-exp)

;; constructors
;; We have three kind of data types, so we made three constructors on for each data type.
;; var-exp : Var -> Lc-exp
;; lambda-exp : Var x Lc-exp -> Lc-exp
;; app-exp : Lc-exp x Lc-exp -> Lc-exp

;; predicates
;; We have three kind of data types, so we made three predicates on for each data type.
;; var-exp? : Lc-exp? -> Bool
;; lambda-exp? : Lc-exp? -> Bool
;; app-exp? : Lc-exp? -> Bool

;; extractors
;; Five pieces of data passed to our constructor
;; For `var-exp` constructor: `var-exp->var`
;; For `lambda-exp` :
;;                    `lambda-exp->bound-var`
;;                    `lambda-exp->body`
;; For `app-exp` :
;;                 `app-exp->rator`
;;                 `app-exp->rand`
;; So we have the following five extractors
;; var-exp->var : Lc-exp -> Var
;; lambda-exp->bound-var : Lc-exp -> Var
;; lambda-exp->bound-body : Lc-exp -> Lc-exp
;; app-exp->rator : Lc-exp -> Lc-exp
;; app-exp->rand : Lc-exp -> Lc-exp

;; occurs-free? : Sym x Lx-exp -> Bool
(define (occurs-free? search-var exp)
  (cond
    [(var-exp? exp)
     (eqv? search-var (var-expr->var exp))]
    [(lambda-exp? exp)
     (and
      (not (eqv? search-var (lambda-exp->bound-var exp)))
      (occurs-free? search-var (lambda-exp->bound-body exp)))]
    [else
     (or
      (occurs-free? search-var (app-exp->rator exp))
      (occurs-free? search-var (app-exp->rand exp)))]))

;; Designing an interface for a recursive  data type
;; [1]. Include one constructor for each kind of data in the data type
;; [2]. Include one predicate for each kind of data in the data type
;; [3]. Include one extractor for each piece of data passed to a constructor of the data type

;; 2.18
;; move to next by specifying position

(define (move-to-next lst pos)
  (if (zero? pos)
      (cons (cadr lst)
            (cons
             (car lst)
             (cddr lst)))
      (cons (car lst)
            (move-to-next (cdr lst)  (- pos 1)))))

;; move to next by specifying element
(define (move-to-next lst elem)
  (if (eqv? elem (car lst))
      (cons (cadr lst)
            (cons (car lst)
                  (cddr lst)))
      (cons (car lst)
            (move-to-next (cdr lst)
                          elem))))

;; number->sequence
(define (number-sequence n)
  (list n '() '()))

(define (current-element lst)
  (car lst))

(define (move-to-left lst)
  (let ([current (car lst)]
        [left (cadr lst)]
        [right (caddr lst)])
    (list
     (car left)
     (cdr left)
     (cons current right))))

(define (move-to-right lst)
  (let ([current (car lst)]
        [left (cadr lst)]
        [right (caddr lst)])
    (list
     (car right)
     (cons current left)
     (cdr right))))

(define (insert-to-left val lst)
  (let ([current (car lst)]
        [left (cadr lst)]
        [right (caddr lst)])
    (list current
          (cons val left)
          right)))

(define (insert-to-right val lst)
  (let ([current (car lst)]
        [left (cadr lst)]
        [right (caddr lst)])
    (list current
          left
          (cons val right))))

(define (at-left-end lst)
  (null? (cadr lst)))

(define (at-right-end lst)
  (null? (caddr lst)))

;; 2.19
;; Bintree = () | (Int BinTree BinTree)
(define (number->bintree n)
  (list n '() '()))

(define (current-element tree)
  (car tree))

(define (move-to-left-son tree)
  (cadr tree))

(define (move-to-right-son tree)
  (caddr tree))

(define (insert-to-left val tree)
  (let ([current (car tree)]
        [left (cadr tree)]
        [right (caddr tree)])
    (list current
          (list val left '())
          right)))

(define (insert-to-right val tree)
  (let ([current (car tree)]
        [left (cadr tree)]
        [right (caddr tree)])
    (list current
          left
          (list val right '()))))

