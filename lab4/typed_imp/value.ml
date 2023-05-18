open Sexprlib

type value = 
  | Unit
  | Bool  of bool
  | Int   of int
  | Array of value array

type func = 
  | PrimFunction of (Loc.loc -> value list -> env -> env * value)
  | UserFunction of Ast.id list * (Ast.id * value) list * Ast.exp
and env = (value, func) Env.t

let rec string_of_value = function
  | Unit       -> "#u"
  | Bool true  -> "#t"
  | Bool false -> "#f"
  | Int i      -> string_of_int i
  | Array a    -> (Array.fold_left (fun out v -> 
    if out = "" 
      then "[" ^ (string_of_value v) 
      else out ^ " " ^ (string_of_value v)
    ) "" a) ^ "]"
    

let rec equal_values l v1 v2 =
  match (v1, v2) with 
    | (Unit, Unit) -> true
    | (Bool b1, Bool b2) -> b1 = b2
    | (Int i1, Int i2) -> i1 = i2
    | (Array a1, Array a2) -> 
        (Array.length a1) = (Array.length a2) &&
        Array.for_all2 (fun v1' v2' -> equal_values l v1' v2') a1 a2
    | _ -> Error.bug_in_type_checker_err l 
             "equality test requires values of the same type"
