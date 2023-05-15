;; Tests of `break`.

(define factorial (x)
  (let ([result 1])
    (begin
      (while
        #t
        (begin
          (if (<= x 0) (break) #u)
          (set result (* result x))
          (set x (- x 1))))
      result)))

(check-expect (factorial 10) 3628800)

;; `break` outside of a `while` loop at the top level.
(check-error (break))

;; `break` crossing function call boundaries.
(define bad-break () (break))
(check-error (bad-break))

(define break-test (n)
  (let ([count n])
    (while (> count 0)
      (begin
        (bad-break)
        (set count (- count 1))))))

(check-error (break-test 10))

;; `break` inside the test case of a `while` loop.
(define ok-break (n)
  (let ([count n] [i 0])
    (begin
      (while (> count 0)
        (begin
          (while 
            ;; This `break` should break out of the *outer* `while` loop.
            (begin (if (= i 3) (break) #u) #t)
              (set i (+ i 1)))
          (set count (- count 1))))
      i)))

(check-expect (ok-break 10) 3)

(define bad-break2 (n)
  (let ([i 0])
    (begin
      (while 
        ;; This `break` is an error.
        (begin (if (= i 3) (break) #u) #t)
          (set i (+ i 1)))
      i)))

(check-error (bad-break2 10))

;; `break` and environments.
(val x 1)
(check-expect 
  (begin
    (while #t
      (let ([x 10])
         (break)))
    x)  ; `x` should be the outer `x`.
  1)

