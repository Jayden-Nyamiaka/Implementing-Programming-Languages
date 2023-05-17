open Sexprlib

type value = 
  | Unit
  | Bool  of bool
  | Int   of int

type func = 
  | PrimFunction of (Loc.loc -> value list -> env -> env * value)
  | UserFunction of Ast.id list * Ast.exp
and env = (value, func) Env.t

let rec string_of_value = function
  | Unit       -> "#u"
  | Bool true  -> "#t"
  | Bool false -> "#f"
  | Int i      -> string_of_int i

let equal_values l v1 v2 =
  match (v1, v2) with 
    | (Unit, Unit) -> true
    | (Bool b1, Bool b2) -> b1 = b2
    | (Int i1, Int i2) -> i1 = i2
    | _ -> Error.bug_in_type_checker_err l 
             "equality test requires values of the same type"
