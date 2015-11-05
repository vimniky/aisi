#lang racket
;; 2.1

;; bigNum representations
;; zero,
;; zero?,
;; preccessor,
;; successor,
;; =?,
;; gt?,
;; gt=?,
;; lt?,
;; lt=?,
;; plus
;; sub
;; mul
;; div
;; .......................

;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (base-zero? n)
  (equal? n '(0)))

(define base-zero '(0))

(define (base-successor n base)
  (if (null? n)
      null
      (let ([l (car n)]
            [rest (cdr n)])
        (cond
          [(< (+ l 1) base) (cons (+ l 1) rest)]
          [(and
            (= (+ l 1) base)
            (null? rest))
           (list 0 1)]
          [else (cons 0 (base-successor rest base))]))))

(define (base-preccessor n base)
  (if (base-zero? n)
      (error "zero doesn't have a preccessor")
      (let ([l (car n)]
            [rest (cdr n)])
        (cond
          [(= l 0)
           (cons (- base 1)
                 (if (and (= 1 (car rest))
                          (null? (cdr rest)))
                     null
                     (base-preccessor rest base)))]
          [else
           (cons (- l 1) rest)]))))

(define (=? a b)
  (if (not (= (length a) (length b)))
      #f
      (let ([va (car a)]
            [vb (car b)]
            [resta (cdr a)]
            [restb (cdr b)])
        (cond
          [(and (= va vb)
                (null? resta)) #t]
          [(= va vb)
           (=? resta restb)]
          [else #f]))))

(define (gt? a b)
  (=? a b)
  #f
  (cond
    [(zero? a) #f]
    [(zero? b) #t]
    [else
     (gt?
      (preccessor a)
      (preccessor b))]))

(define (lt? a b)
  (cond
    [(=? a b) #f]
    [(zero? a) #t]
    [(zero? b) #f]
    [else
     (lt?
      (preccessor a)
      (preccessor b))]))

(define (gt=? a b)
  (or
   (=? a b)
   (gt? a b)))

(define (lt=? a b)
  (or
   (=? a b)
   (lt? a b)))

(define zero '(0))

(define (zero? n)
  (equal? n '(0)))

(define (plus a b)
  (if (zero? b)
      a
      (plus
       (successor a)
       (preccessor b))))

(define (sub a b)
  (if (zero? b)
      a
      (sub
       (preccessor a)
       (preccessor b))))

(define (mul a b)
  (if (=? (successor zero) b)
      a
      (plus a
            (mul a
                 (preccessor b)))))

(define (div a b)
  (cond
    [(zero? b) (error "Denominator can't be zero")]
    [(lt? a b) zero]
    [(=? a b) (successor zero)]
    [(=? (successor zero) b) a]
    [else
     (successor
      (div (sub a b) b))]))

;; ;; base2
;; (define (successor n)
;;   (base-successor n 2))

;; (define (preccessor n)
;;   (base-preccessor n 2))

;; ;; base4
;; (define (successor n)
;;   (base-successor n 4))

;; (define (preccessor n)
;;   (base-preccessor n 4))

;; base10
(define (successor n)
  (base-successor n 10))

(define (preccessor n)
  (base-preccessor n 10))

;; 2.1
(define one (successor zero))

(define (factorial n)
  (if (=? one n)
      one
      (mul n (factorial (preccessor n)))))
