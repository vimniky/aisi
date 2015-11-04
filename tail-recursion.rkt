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
