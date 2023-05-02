(* Values and environments. *)

open Sexprlib

(* An environment (name/value mapping).  Mappings are mutable. *)
type env
type value = 
  | PrimFuncVal of (Loc.loc -> value list -> value)
  (* Note that user-defined functions capture an environment. *)
  | UserFuncVal of Ast.id list * Ir.expr * env
  | UnitVal (* Unit value--value of e.g. `set` and `while`. *)
  | BoolVal of bool
  | IntVal  of int
  | SymVal  of Ast.id
  | NilVal  (* Empty list. *)
  | PairVal of value * value
  (* For temporarily putting in environments to allow recursion.
   * Any attempt to actually evaluate this will be an error. *)
  | Unspecified of Ast.id

(*
 * Operations on values.
 *)

(* Pretty-print the given value. *)
val string_of_value : value -> string

(* Determine whether a value counts as "true". *)
val truthy : Loc.loc -> value -> bool

(*
 * Operations on environments.
 *)

(* Check to see if a name exists in an environment. *)
val mem : Ast.id -> env -> bool

(* Lookup the given name in the environment, throwing an Error.UScheme_err if
 * not found. *)
val lookup : Loc.loc -> env -> Ast.id -> value

(* Modify the value of the given variable in-place, throwing an
 * Error.UScheme_err if not found. *)
val set : Loc.loc -> env -> Ast.id -> value -> unit

(* Add a binding to an environment. *)
val bind : env -> Ast.id -> value -> env

(* Make a new environment with the given names and values. *)
val make_env : (Ast.id * value) list -> env

