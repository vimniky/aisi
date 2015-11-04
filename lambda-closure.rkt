;; version 1 --> after being executed , it will just be reclaimed by the
;; gabage collection
(let [(count 0)]
  [lambda ()
    (set! count (+ count 1))])

;; version two --> capture the closure
(define culosure-capturer
  (let [(count 0)] ; let form will be executed , and return the closure created
    [lambda ()     ; by the lambda
      (set! count (+ count 1))]))

;; version three
(define (make-closure)  ;define a procedure , every time make-closure was
  (let [(count 0)]      ;call , a new closure was created
    [lambda ()
      (set! count (+ count 1))]
    count))

;; now let's create some closure
(define closure-1 (make-closure))
(define closure-2 (make-closure)) ;...

;; version three varients
(define make-closure-1
  (lambda ()
    (let [(count 0)]
      (lambda ()
        (set! count (+ count 1)))
      count)))

(define make-closure-2
  (lambda ()
    (lambda ()
      (let [(count 0)]
        (set! count (+ count 1))
        count))))

;; variable are just local variables that get their initial value
;; in a special way
(define (make-count-3 count)
  (lambda ()
    (set! (+ count 1))
    count))
