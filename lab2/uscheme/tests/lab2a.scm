;; Tests for part A of assignment 2.
;; Testing the new forms added to the language.

;; = works with ints, nil, unit, booleans, and symbols

(check-expect (= 42 42) #t)
(check-expect (= 42 -42) #f)
(check-expect (= #t #t) #t)
(check-expect (= #f #f) #t)
(check-expect (= #t #f) #f)
(check-expect (= #t 10) #f)
(check-expect (= #u #u) #t)
(check-expect (= #u #f) #f)
(check-expect (= nil nil) #t)
(check-expect (= nil #u) #f)
(check-expect (= 'a 'a) #t)
(check-expect (= 'a 'aaa) #f)

;; let, let*, letrec with multiple expressions in body
(check-expect
  (let ([x 10] [y 20])
    (set x (+ x 42))
    (set y (- y 54))
    (+ x y))
  18)

(check-expect
  (let* ([x 10] [y (* x 2)])
    (set x (+ x 42))
    (set y (- y 54))
    (+ x y))
  18)

(check-error
  (let* ([y (* x 2)] [x 10])
    (set x (+ x 42))
    (set y (- y 54))
    (+ x y)))

(check-expect
  (letrec
    ([even? (lambda (n) (if (= n 0) #t (odd?  (- n 1))))]
     [odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))])
    (odd? 1001))
  #t)

(check-expect
  (letrec
    ([even? (lambda (n) (odd? n))]   ; nope
     [odd?  (lambda (n) (even? n))]) ; nope
    (set even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
    (set odd?  (lambda (n) (if (= n 0) #f (even? (- n 1)))))
    (even? 1001))
  #f)

;; lambda with multiple expressions in body

(check-expect
  ((lambda (x y z)
     (set x (+ x 1))
     (set y (- y 2))
     (set z (* z 3))
     (+ (+ x y) z))
   1 2 3)
  11)

;; define with multiple expressions in body

(define deftest1 (x y z)
  (set x (+ x 1))
  (set y (- y 2))
  (set z (* z 3))
  (+ (+ x y) z))

(check-expect (deftest1 1 2 3) 11)

;; while with multiple expressions in body

(define sumto (n)
  (let ([r 0])
    (while (> n 0)
      (set r (+ r n))
      (set n (- n 1)))
    r))

(check-expect (sumto 10) 55)

;; cond

(check-expect (cond) #u)
(check-expect (cond [(= 10 10)]) #u)

(define abs (n)
  (cond [(= n 0) 0]
        [(< n 0) (- 0 n)]
        [#t n]))

(check-expect (abs 0)    0)
(check-expect (abs 10)  10)
(check-expect (abs -10) 10)

(define cond-test (n)
  (cond [(= n 0) (set n (+ n 42)) n]
        [(< n 0) (set n (- 0 n)) n]
        [#t (set n (* n 2)) n]))

(check-expect (cond-test 0) 42)
(check-expect (cond-test 10) 20)
(check-expect (cond-test -10) 10)

;; lambda with arbitrary numbers of arguments

(val blub (lambda x (cons 'item x)))

(check-expect (blub 1 2 3) '(item 1 2 3))

(val blab (lambda (x y . z) (cons x (cons y (cons 'foo z)))))

(check-expect (blab 1 2 3) '(1 2 foo 3))

;; define with arbitrary numbers of arguments

(define foo x (cons 'item x))

(check-expect (foo 1 2 3) '(item 1 2 3))

(define bar (x y . z) (cons x (cons y (cons 'foo z))))

(check-expect (bar 1 2 3) '(1 2 foo 3))

;; set-car and set-cdr

(val x (cons 1 2))
(set-car x 42)
(check-expect (car x) 42)
(set-cdr x 54)
(check-expect (cdr x) 54)
(check-expect (car x) 42)

;; and and or

(check-expect (and) #t)
(check-expect (or) #f)
(check-expect (and 42) 42)
(check-expect (or 42) 42)
(check-expect (and 10 20 30) 30)
(check-expect (and 10 0 30) 30)
(check-expect (and 10 nil 30) 30)
(check-expect (and 10 #f 30) #f)
(check-expect (and 10 #f) #f)
(check-expect (or 10 20 30) 10)
(check-expect (or #f 10 0) 10)
(check-expect (or 10 nil 30) 10)
(check-expect (or #f 30) 30)

;; valrec

(valrec 
  [even (lambda (n) (if (= n 0) #t (odd (- n 1))))]
  [odd  (lambda (n) (if (= n 0) #f (even (- n 1))))])

(check-expect (even 1000) #t)
(check-expect (even 1001) #f)
(check-expect (odd 1000) #f)
(check-expect (odd 1001) #t)
