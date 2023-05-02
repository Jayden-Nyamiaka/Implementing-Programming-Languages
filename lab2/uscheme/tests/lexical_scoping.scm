;
; Test that lambdas can capture variables.
;

(val f 
  (let ((x 1))
    (lambda () x)))
(val x 2)

(check-expect x 2)
(check-expect (f) 1)
(check-expect x 2)
