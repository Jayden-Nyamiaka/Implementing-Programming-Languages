;; Test of tail call optimization.
;; Change `trace-fs` argument from 1 to 2 to get a full trace.

(define ret42 () (return 42))
(define test () (+ 1 (let ([x 1]) (ret42))))
(check-expect (test) 43)
(check-expect (let ([x 1]) (ret42)) 42)

;; Not tail-recursive.
(define fact (n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
(trace-fs 1)
(check-expect (fact 10) 3628800)
(check-expect (trace-fs 0) 20)  ;; should be 20 frames on the stack

;; Tail-recursive.
(define fact-iter (n r)
  (if (= n 0)
      r
      (fact-iter (- n 1) (* n r))))

;; Check that tail call optimization works.
(check-expect (trace-fs 1) 0)
(check-expect (fact-iter 10 1) 3628800)
(check-expect (trace-fs 0) 1)

;; This should also be tail-recursive,
;; though it's much less obvious.
(define fact-iter2 (n r)
  (if (= n 0)
      r
      (let ([n2 (- n 1)]
            [r2 (* n r)])
         (fact-iter2 n2 r2))))

;; Check that tail call optimization works.
(check-expect (trace-fs 1) 0)
(check-expect (fact-iter2 10 1) 3628800)
(check-expect (trace-fs 0) 1)

;;
;; Test that `begin` expressions don't break tail call optimizations.
;;

;; This should be tail-recursive.
(define countdown1 (m n)
   (if (= m 0)
       n
       (countdown1 (- m 1) (+ n 1))))

;; Check that tail call optimization works.
(check-expect (trace-fs 1) 0)
(check-expect (countdown1 100 0) 100)
(check-expect (trace-fs 0) 1)

;; This should be tail-recursive,
;; but the `begin` can mess up tail call optimization
;; if it's not handled properly.
(define countdown2 (m n)
   (if (= m 0)
       n
       (begin
         (countdown2 (- m 1) (+ n 1)))))

;; Check that tail call optimization works.
(check-expect (trace-fs 1) 0)
(check-expect (countdown2 100 0) 100)
(check-expect (trace-fs 0) 1)

;; This should be tail-recursive,
;; but the `begin` can again mess up tail call optimization
;; if it's not handled properly.
(define countdown3 (m n)
   (if (= m 0)
       n
       (begin
         #u
         (countdown3 (- m 1) (+ n 1)))))

;; Check that tail call optimization works.
(check-expect (trace-fs 1) 0)
(check-expect (countdown3 100 0) 100)
(check-expect (trace-fs 0) 1)

;;
;; Other tests relating to controlling stack growth.
;;

;; Test that nested `let`s don't grow the stack.
(check-expect (trace-fs 1) 0)
(check-expect
  (let ([a 1])
    (let ([b 2])
      (let ([c 3])
        (let ([d 4])
          (let ([e 5])
            (let ([f 6])
              (let ([g 7])
                (let ([h 8])
                  (+ (+ (+ a b) (+ c d)) (+ (+ e f) (+ g h)))))))))))
  36)
(check-expect (trace-fs 0) 1)

;; Test that `LetEnvFrame`/`CallEnvFrame` pushes behave correctly.
(define weird-test (n r)
  (if (= n 0)
      r
      (+ 1
        (let ([n2 (- n 1)]
              [r2 (* n r)])
          (weird-test n2 r2)))))
(check-expect (trace-fs 1) 0)
(check-expect (weird-test 10 1) 3628810)
(check-expect (trace-fs 0) 21)

