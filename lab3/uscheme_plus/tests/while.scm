;
; Test while loops and imperative programming.
;

(define factorial (x)
  (let ((result 1))
    (begin
      (while
        (> x 0)
        (begin
          (set result (* result x))
          (set x (- x 1))))
      result)))

(check-expect (factorial 0) 1)
(check-expect (factorial 1) 1)
(check-expect (factorial 10) 3628800)
