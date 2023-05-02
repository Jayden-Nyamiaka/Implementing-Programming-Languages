;
; Test that boolean operations work as expected.
;

; #t/#f
(check-expect (if #t 1 2) 1)
(check-expect (if #f 1 2) 2)

; not
(check-expect (not #t) #f)
(check-expect (not #f) #t)
(check-expect (not 0) #f)
(check-expect (not 1) #f)

