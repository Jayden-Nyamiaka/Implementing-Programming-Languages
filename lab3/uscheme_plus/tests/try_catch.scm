; Test exception-handling functionality.

; Define a version of division that throws on zero denominator.
(val old-/ /)
(define / (x y)
  (if (= y 0)
      (throw 0)
      (old-/ x y)))

; Use the id function as a simple handler.
(define id (x) x)

; Use succ if we want another handler.
(define succ (e) (+ e 1))

; A non-function in the `catch` position should be an error.
(check-error (try-catch (throw 0) 1))

; Test calling a throwing function.
(check-expect (try-catch (/ 10 0) id) 0)
(check-expect (try-catch (/ 10 5) id) 2)

; Add a layer to the call stack.
(check-expect (try-catch (id (/ 10 0)) id) 0)
(check-expect (try-catch (id (/ 10 5)) id) 2)

; Try making sure the handler actually gets called when it's supposed to.
(check-expect (try-catch (/ 10 0) succ) 1)
(check-expect (try-catch (/ 10 5) succ) 2)

; And not when it doesn't.
(val canary 0)
; Use this as a handler to check that the handler doesn't get called.
(define change-canary (e)
  (begin
    (set canary 1)
    e))

(check-expect (try-catch (/ 10 5) change-canary) 2)
(check-expect canary 0)

; Try nesting try/catches.
(check-expect
  (try-catch
    (try-catch
      (/ 10 0)
      id)
    succ)
  0)

; Try having catch blocks throw.
(check-expect
  (try-catch
    (try-catch
      (/ 10 0)
      (lambda (err) (throw 5)))
    id)
  5)

; Test that catching an exception restores environments correctly.
; This is an incredibly subtle test.
(define test (n)
  (let ([x 10])
    (throw (+ n x))))
(check-expect
  (let ([x 42])
    (+
      (try-catch
        (test 21)  ; should throw 31
        (lambda (e) (+ e x))) ; 31 + 42 = 73
      x)) ; 73 + 42 = 115
  115)

