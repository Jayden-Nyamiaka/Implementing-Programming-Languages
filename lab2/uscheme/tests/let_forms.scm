;
; Test that the different forms of `let` work correctly.
;

; `let` should not allow definitions to depend on each other.
(check-error (let ((x 2) (y x)) y))

; `let*` should.
(check-expect (let* ((x 2) (y x)) y) 2)

; `let` and `let*` should not allow recursion.
(check-error 
  (let ((factorial 
           (lambda (n)
              (if (= n 0)
                  1
                  (* n (factorial (- n 1)))))))
    (factorial 10)))
(check-error 
  (let* ((factorial 
            (lambda (n)
               (if (= n 0)
                   1
                   (* n (factorial (- n 1)))))))
  (factorial 10)))

; `letrec` should.
(check-expect 
  (letrec ((factorial 
              (lambda (n)
                (if (= n 0)
                   1
                   (* n (factorial (- n 1)))))))
  (factorial 10))
  3628800)

; `letrec` and mutual recursion.
(check-expect
  (letrec ([even? (lambda (n) (if (= n 0) #t (odd?  (- n 1))))]
           [odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))])
	(even? 1001))
  #f)

; `letrec` errors.
(check-error
  (letrec ((x 10) (y x)) (+ x y)))  ; prematurely read x
(check-error
  (letrec ((x (begin (set y 10) y)) (y 42)) (+ x y)))  ; prematurely write x

; `let` with multiple body forms.
(check-expect
  (let ([x 42]) (+ x 1) (* x 2)) 84)
; This is a type error due to extraneous parentheses around the `let` body:
(check-error
  (let ([x 42]) ((+ x 1) (* x 2))))

