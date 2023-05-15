;
; Test that `set` works and interacts properly with scoping.
;

; Simply setting a global variable.
(val global 0)
(set global 1)
(check-expect global 1)

; Setting a local variable.
(check-expect
  (let ((local 1))
    (begin
      (set local 2)
      local))
  2)

; Setting a local shadowing a global.
(val shadowed 0)
(check-expect
  (let ((shadowed 1))
    (begin
      (set shadowed 2)
      shadowed))
  2)
; Check that the shadowed global was unaffected.
(check-expect shadowed 0)

; Setting a local shadowing another local.
(check-expect
  (let ((shadowed-local 1))
    (* (let ((shadowed-local 2))
         (begin
           (set shadowed-local 3)
           shadowed-local))
      shadowed-local))
  3)
