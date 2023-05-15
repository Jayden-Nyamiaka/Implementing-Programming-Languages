; Test that deep recursion doesn't blow the stack.
(define recurse-n (n)
  (if (= n 0)
    0
    (recurse-n (- n 1))))

(check-expect (recurse-n 100000) 0)
