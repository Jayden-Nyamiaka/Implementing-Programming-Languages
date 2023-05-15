; Tests involving `return`.

; Check that early returns actually return the expected value.
(define ret-early (x) (+ (return x) 10))
(check-expect (ret-early 10) 10)

; Check that early returns don't run into errors by evaluating the
; whole body.
(define ret-avoids-errors (x) (+ (return x) #t))
(check-expect (ret-avoids-errors 10) 10)

; Check that an early return will break an infinite loop.
(define ret-infinite (x)
  (while #t
    (if (<= x 0) (return x)
      (set x (- x 1)))))
(check-expect (ret-infinite 10) 0)

; Check that a `return` outside a function is an error.
(check-error (return #u))
