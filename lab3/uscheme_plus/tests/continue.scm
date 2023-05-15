;; Tests of `continue`.

(define test-continue (n)
  (let ([result 0])
    (begin
      (while (> n 0)
        (begin
          (set n (- n 1))
          (if (= (mod n 2) 0)
              (continue)
              (set result (+ result n)))))
      result)))

(check-expect (test-continue 10) 25)

;; `continue` outside of a `while` loop at the top level.
(check-error (continue))

;; `continue` crossing function call boundaries.
(define bad-continue () (continue))
(check-error (bad-continue))

(val count 10)
(define continue-test ()
  (while (> count 0)
    (begin
      (bad-continue)
      (set count (- count 1)))))

(check-error (continue-test))

;; `continue` inside the test case of a `while` loop.
(define ok-continue (n)
  (let ([count n] [i 0])
    (begin
      (while (> count 0)
        (begin
          (while 
            ;; This `continue` should continue out of the *outer* `while` loop.
            (begin (if (= i 3) (continue) (set count (- count 100))) #t)
              (set i (+ i 1)))
          (set count (- count 1))))
      count)))

(check-expect (ok-continue 10) -290)

(define bad-continue2 (n)
  (let ([i 0])
    (begin
      (while 
        ;; This `continue` is an error.
        (begin (if (= i 3) (continue) #u) #t)
          (set i (+ i 1)))
      i)))

(check-error (bad-continue2 10))

