; Test lists, and the list functions in the initial basis.

; cons, car, cdr.
(check-expect (cons 1 nil) (list 1))
(check-expect (car (cons 1 nil)) 1)
(check-expect (cdr (cons 1 nil)) nil)
(check-expect (cdr (cons 1 2)) 2)
(check-error (car nil))
(check-error (car 1))
(check-error (cdr nil))
(check-error (cdr 1))

; equal?
(check-expect (equal? nil nil) #t)
(check-expect (equal? 1 1) #t)
(check-expect (equal? nil 1) #f)
(check-expect (equal? (list 1 2 3) (list 1 2 3)) #t)
(check-expect (equal? (list (list 1 2) (list 2 3))
                      (list (list 1 2) (list 2 3)) )
              #t)
(check-expect (equal? (list (list 1 2) (list 2 3))
                      (list (list 1 2) (list 3 3)) )
              #f)

; reverse
(check-expect (reverse nil) nil)
(check-expect (reverse (list 1 2 3)) (list 3 2 1))

; revapp
(check-expect (revapp nil nil) nil)
(check-expect (revapp nil (list 1 2 3)) (list 1 2 3))
(check-expect (revapp (list 1 2 3) nil) (list 3 2 1))
(check-expect (revapp (list 3 2 1) (list 4 5 6)) (list 1 2 3 4 5 6))

; append
(check-expect (append nil nil) nil)
(check-expect (append nil (list 1 2 3)) (list 1 2 3))
(check-expect (append (list 1 2 3) nil) (list 1 2 3))
(check-expect (append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

; find
(val test-alist (list (cons 1 2) (cons 2 3) (cons 3 4)))

(check-expect (find 0 test-alist) #f)
(check-expect (find 1 test-alist) 2)
(check-expect (find 2 test-alist) 3)
(check-expect (find 3 test-alist) 4)

; bind
(val test-alist# (bind 0 1 test-alist))
(check-expect (find 0 test-alist#) 1)
(check-expect (find 3 test-alist#) 4)

(val test-alist## (bind 1 0 test-alist#))
(check-expect (find 1 test-alist##) 0)
(check-expect (find 3 test-alist##) 4)
