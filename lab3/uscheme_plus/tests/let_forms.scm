;
; Test that the different forms of `let` work correctly.
;

; `let` should not allow definitions to depend on each other.
(check-error (let ((x 2) (y x)) y))

; `let*` should.
(check-expect (let* ((x 2) (y x)) y) 2)

; `let` and `let*` should not allow recursion.
(check-error (let
  ((factorial (lambda (x)
                (if (<= x 1)
                  1
                  (* x (factorial (- x 1)))))))
  (factorial 10)))
(check-error (let*
  ((factorial (lambda (x)
                (if (<= x 1)
                  1
                  (* x (factorial (- x 1)))))))
  (factorial 10)))

; `letrec` should.
(check-expect (letrec
  ((factorial (lambda (x)
                (if (<= x 1)
                  1
                  (* x (factorial (- x 1)))))))
  (factorial 10))
  3628800)

; All of them should evaluate their arguments left-to-right.
(check-expect 
  (let ((a 0))
    (let ((b (set a (+ a 1)))
          (c (set a (* a 2))))
      a))
  2)
(check-expect 
  (let ((a 0))
    (let* ((b (set a (+ a 1)))
           (c (set a (* a 2))))
      a))
  2)
(check-expect 
  (let ((a 0))
    (letrec ((b (set a (+ a 1)))
              (c (set a (* a 2))))
      a))
  2)
