open Sexprlib

open Sexpr
open Env

(* Convert an S-expression to a uscheme value. *)
let rec value_of_sexpr e = 
  match e with
    | Int (_, i) -> IntVal i

    | Id (_, "#t")  -> BoolVal true
    | Id (_, "#f")  -> BoolVal false
    | Id (_, "#u")  -> UnitVal
    | Id (_, "nil") -> NilVal
    | Id (_, s)     -> SymVal s

    | List (_, lst) -> value_of_sexpr_list lst

and value_of_sexpr_list lst =
  match lst with
    | [] -> NilVal
    | e :: es -> 
      PairVal ((value_of_sexpr e), (value_of_sexpr_list es))

