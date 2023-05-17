(*
 * The initial basis.
 *)

(* The type of interpreter environments: values and types. *)
type env = 
  {
    type_env : Typecheck.env;
    val_env  : Value.env;
  }

val basis : env
val user_basis : string

