(define (add-list lst sum)
  (if (null? lst)
      sum
      (add-list (cdr (+ sum (car lst))))))

;; friendly interface
(define (sum-up-list lst)
  (add-list lst 0))

;; using letrec
(define (sumup-list lst)
  (letrec [(add-list
            (lambda (lst sum)
              (if [null? lst]
                  sum
                  [add-list (cdr lst)
                            (+ sum (car lst))])))]
    (add-list lst 0)))

;; using named let
(define (sumup-list lst)
  (let loop [(lst lst) (sum-so-far 0)]
    [if (null? lst)
        sum-so-far
        (loop (cdr lst (+ sum-so-far (car lst))))]))

;; list length
(define (length lst)
  (let loop [(lst lst) (length-so-far 0)]
    (if [null? lst]
        length-so-far
        [loop (cdr lst) (+ length-so-far (car lst))])))

;; generic reduce
(define (reduce fn base-value lst)
  (if (null? lst)
      base-value
      (reduce (fn (car lst))
              base-value
              (cdr lst))))

;; make-reduce
(define (make-reduce fn base-value)
  (lambda (lst)
    (reduce fn base-value lst)))

;; make-reduce with letrec
(define (make-reduce fn base-value)
  (letrec [(reduce (lambda (lst)
                     (if (null? lst)
                         base-value
                         (fn (car lst)
                             (reduce (cdr lst))))))]
    reduce))


;; copy a list
(define (lst-copy lst)
  (reduce cons ()' lst))

;; copy a list
(define (list-copy lst)
  (let loop [(cons-so-far '()) (lst lst)]
    (cond [(null? lst) cons-so-far]
          [else (list-copy (cons (car lst) cons-so-far) (cdr lst)) ])))

;; append
(define (append lst1 lst2)
  (cons
   [reduce cons '() lst1]
   [reduce cons '() lst2]))

(define (append lst1 lst2)
  (reduce cons lis1 lis2))
