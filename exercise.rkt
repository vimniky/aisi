#lang racket

;; list-of-int
;; ==> (Int . List-of-int)
;; ==> (14 . List-of-int)
;; ==> (14 . ())

;; List-of-int
;; ==> (Int . List-of-int)
;; ==> (-7 . List-of-int)
;; ==> (-7 . (Int . List-of-int))
;; ==> (-7 . (3 . List-of-int))
;; ==> (-7 . (3 . (Int . List-of-int)))
;; ==> (-7 . (3 . (14 . List-of-int)))
;; ==> (-7 . (3 . (14 . ())))

(define (remove-first s los)
  (if (null? los)
      '()
      (if (eqv? s (car los))
          (cdr los)
          (cons (car los) (remove-first s (cdr los))))))

(define (remove-all-occurs s los)
  (if (null? los)
      '()
      (if (eqv? s (car los))
          (remove-all-occurs s (cdr los))
          (cons (car los) (remove-all-occurs s (cdr los))))))))

;; occurs-free?
(occurs-free? 'x 'x) ; #t
(occurs-free? 'x 'y) ; #f
(occurs-free? 'x (lambda (x) (x y))) ; #f
(occurs-free? 'x ((lambda (x) x) (x y))) ; #t
(occurs-free? ’x ’(lambda (y) (lambda (z) (x (y z))))) ;#t

;; lambda-calculus
;; LxExp ::= Identify
;;       ::= (lambda (Identify) LcExp)
;;       ::= (LcExp LcExp)

;; [1]
;; If the expression of e is a variable, then the variable x occurs free in e if and
;; only if x is the same as e
;; [2]
;; If the expresson e is them form (lambda (y) e'), then then variable occurs free in
;; e if and only if y is different from x and x occurs free in e
;; [3]
;; If the expression e is of the form (e1 e2), then x occurs free in e if
;; and only if it occurs free in e1 or e2.

(define (occurs-free? var exp)
  (cond
    [(symbol? exp) (eqv? var exp)]
    [(eqv? (var exp) 'lambda)
     (and (not (eqv? var (car (cadr exp))))
          (occurs-free var (car (caddr exp))))]
    [else
     (or
      (occurs-free? var (car exp))
      (occurs-free? var (cdr exp)))]))

;; subst
;; [1]
;; S-list ::= ({S-exp}*)
;; S-exp  ::= Symbol | S-list
;; [2]
;; S-list ::= ()
;;        ::= (S-exp . S-list)
;; S-Exp  ::= Symbol | S-list
;; [3]
;; The grammer input contains two nonterminals, S-list and S-exp. Therefore
;; we will have two procedures one for dealing with each
;; subst
;; Sym x Sym x S-list -> S-list
(define (subst new old s-list)
  (if (null? s-list)
      '()
      (cons
       (subst-in-s-exp new old (car s-list))
       (subst new old (cdr s-list)))))
;; subst-in-exp
;; Sym x Sym x S-list -> S-list
(define (subst-in-s-exp new old s-exp)
  (if (symbol? s-exp)
      (if (eqv? old s-exp)
          new
          s-exp)
      (subst new old s-exp)))

;; 1.12
(define (subst new old slist)
  (cond
    [(null? slist) '()]
    [(symbol? slist) (if (eqv? old slist) new slist)]
    [else
     (cons
      (subst new old (car slist))
      (subst new old (cdr slist)))]))

;; 1.13
;; S-list ::= ({S-exp}*)
;; S-exp  ::= Symbol | S-list
(define (subst new old slist)
  (if (null? slist)
      '()
      (map
       (lambda (sexp)
         (if (symbol? sexp)
             (if (eqv? old sexp)
                 new
                 sexp)
             (subst new old sexp)))
       slist)))
;; Follow The Grammar !
;; When defining a procedure that operates on inductively defined data
;; the structure of the program should be patterned after the structure of the data
;; More precisely
;; [1]
;; Write one procedure for each nonterminal in the grammer. The procedure
;; will be responsible for handling the data corresponding to that nonterminal, and nothing more.
;; [2]
;; In each procedure, write one alternative for each production corresponding to that nonterminal
;; You may need additional case structure, but this will get you started. For each nonterminal that appears
;; in the right-hand side, write a recursive call to the procedure for that nontermial.

;; 1.14
;; number-elements
(define (number-elements-from ls n)
  (if (null? ls) '()
      (cons (cons n (car ls))
            (number-elements-from (cdr ls) (+ n 1)))))

(define (number-elements ls)
  (number-elements-from ls 0))

;; 1.15
(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (- n 1) x))))

;; tail-recursive / auxiliary
(define (duple-from n x result)
  (if (zero? n)
      result
      (duple-from (- n 1) x (cons x result))))
(define (duple n x)
  (duple-from n x '()))

;; 1.16
(define (revert lst)
  (if (null? lst)
      '()
      (map
       (lambda (l)
         (let [(a (car l))
                (b (cadr l))]
           (list b a)))
       lst)))

(define (revert lst)
  (if (null? lst)
      null
      (let* [(pair (car lst))
             (a (car pair))
             (b (cadr pair))]
        (cons
         (cons b a)
         (revert (cdr lst))))))

;; 1.17
(define (down lst)
  (if (null? lst)
      null
      (map
       (lambda (l)
         (if (not (pair? l))
             (list l)
             l))
       lst)))

(define (down lst)
  (if (null? lst)
      null
      (if (not (pair? (car lst)))
          (cons (list (car lst))
                (down (cdr lst)))
          (cons (car lst)
                 (down (cdr lst))))))

;; 1.18
(define (swapper s1 s2 slist)
  (if (null? slist)
      null
      (let [(v (car slist))]
        (cond
          [(pair? v)
           (cons (swapper s1 s2 (car slist))
                 (swapper s1 s2 (cdr slist)))]
          [(eqv? s1 v)
           (cons s2 (swapper s1 s2 (cdr slist)))]
          [(eqv? s2 v)
           (cons s1 (swapper s1 s2 (cdr slist)))]
          [else (cons (car slist)
                      (swapper s1 s2 (cdr slist)))]))))

(define (swapper s1 s2 slist)
  (if (null? slist)
      null
      (map (lambda (v)
             (cond
               [(pair? v)
                (swapper s1 s2 v)]
               [(eqv? s1 v)
                s2]
               [(eqv? s2 v) s1]
               [else v]))
           slist)))

;; 1.19
(define (list-set lst n x)
  (if (null? lst)
      (error "Oooo Set failed !")
      (if (zero? n)
          (cons
           x
           (cdr lst))
          (cons
           (car lst)
           (list-set (cdr lst) (- n 1) x)))))
;;
(define (map-with-index fn lst)
  (map-with-index-from fn lst 0))

(define (map-with-index-from fn lst startVal)
  (if (null? lst)
      null
      (cons
       (fn (car lst) startVal)
       (map-with-index-from fn (cdr lst) (+ startVal 1)))))

;; 1.20
(define (count-occurrences s slist)
  (if (null? slist)
      0
      (if (not (pair? (car slist)))
          (if (eqv? s (car slist))
              (+ 1 (count-occurrences s (cdr slist)))
              (count-occurrences s (cdr slist)))
          (+
           (count-occurrences s (car slist))
           (count-occurrences s (cdr slist))))))
;; using fold ...

;; 1.21
(define (product sos1 sos2)
  (if (null? sos1)
      null
      (append
       (pair-with sos2 (car sos1))
       (product (cdr sos1) sos2))))

(define (pair-with sos v)
  (if (null? sos)
      null
      (cons
       (cons v (car sos))
       (pair-with (cdr sos) v))))

;; using map
(define (product sos1 sos2)
  (map
   (lambda (x)
     (map
      (lambda (y)
        (cons x y))
      sos2))
   sos1))

;; 1.22
(define (filter-in pred lst)
  (if (null? lst)
      null
      (if (pred (car lst))
          (cons
           (car lst)
           (filter-in pred (cdr lst)))
          (filter-in pred (cdr lst)))))
;; 1.23
(define (list-index pred lst)
  (list-index-from pred lst 0))

(define (list-index-from pred lst n)
  (if (null? lst)
      #f
      (if (pred (car lst))
          n
          (list-index-from pred (cdr lst) (+ n 1)))))
;; 1.24
(define (every? pred lst)
  (if (null? lst)
      #t
      (if (not (pred (car lst)))
          #f
          (every? pred (cdr lst)))))

;; 1.25
(define (exists? pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          #t
          (exists? pred (cdr lst)))))

;; 1.26
(define (up lst)
  (if (null? lst)
      null
      (if (pair? (car lst))
          (append
           (car lst)
           (up (cdr lst)))
          (cons
           (car lst)
           (up (cdr lst))))))

;; 1.27
(define (flatten slist)
  (if (null? slist)
      null
      (if (not (pair? (car slist)))
          (cons
           (car slist)
           (flatten (cdr slist)))
          (append
           (flatten (car slist))
           (flatten (cdr slist))))))
;;;;;;;;;;;;;;;;;;;;
;; related to sorting and tree
;;;;;;;;;;;;;;;;;;;;
;; 1.28 merge-sort
;; 1.29 sort
;; 1.30 sort/predicate
;; 1.31 iterior/node
;; 1.32 double/tree
;; 1.32 mark-leaves-with-red-depth
;; 1.34 path
;; 1.35 number-leaves
;; 1.36
