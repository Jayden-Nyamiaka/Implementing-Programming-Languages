;; Tests of scoping.
;; Thanks to Enrico Borba for pointing this out.

(define f ()
  (begin
    (while #t
      (let ([result 1]) 
        (break))) ;; break inside let
    result))      ;; checking `result` outside of let

(check-error (f))

;; OK, so maybe the name of this test script isn't 100% accurate.
;; Testing similar scoping issues with `continue`.
(define g ()
  (let ([n 10])
    (begin
      (while (> n 0)
        (let ([result 1])
          (begin
            (set n (- n 1))
            (continue))))
      result)))

(check-error (g))
