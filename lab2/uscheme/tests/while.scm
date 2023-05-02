;
; Test while loops and imperative programming.
;

(define factorial (n)
  (let ((result 1))
    (begin
      (while
        (> n 0)
        (begin
          (set result (* result n))
          (set n (- n 1))))
      result)))

(check-expect (factorial 10) 3628800)
