;; maker
(define (make-point x y)
  (vector 'point x y))
;; checker
(define (point? obj)
  (eq? (vector-ref obj 0) 'point ))

;;; accessors
;; getters
(define (point-x obj)
  (if (not (point? obj))
      (error "obj is not a point type")
      (vector-ref obj 1)))

(define (point-y obj)
  (if (not (point? obj))
      (error "obj is not a point type")
      (vector-ref obj 2)))

;; setters
(define (point-x! obj value)
  (if (not (point? obj))
      (error "obj is not a point type")
      (vector-set! obj 1 value)))

(define (point-y! obj value)
  (if (not (point? obj))
      (error "obj is not a point type")
      (vector-set!  obj 2 value)))
