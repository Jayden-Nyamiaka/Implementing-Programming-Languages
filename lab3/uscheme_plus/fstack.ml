(* The stack type records its size to allow for overflow checks. *)
type 'a t = {
  contents : 'a list; 
  size : int
}

exception Underflow
exception Overflow

let max_size = ref 1000

let set_max_size n = max_size := n

let create () = { contents = []; size = 0 }

let contents s = s.contents

let depth s = s.size

let is_empty = function
  | { contents = []; _ } -> true
  | _ -> false

let push v { contents=c; size=s } = 
  if s >= !max_size
  then raise Overflow
  else { contents = v :: c; size = s + 1 }

let pop = function
  | { contents = []; _ } -> raise Underflow
  | { contents = v :: vs; size = s } ->
    (v, { contents = vs; size = s - 1 })

let top = function
  | { contents = []; _ } -> raise Underflow
  | { contents = v :: _; _ } -> v

