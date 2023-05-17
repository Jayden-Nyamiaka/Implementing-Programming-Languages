(*
 * Environments.
 *
 * Contains data structures and operations for keeping track of Imp variables
 * and functions, including dynamic updates.
 *)

open Sexprlib

(* Map from strings to arbitrary types.
 * The key type is exposed for ease of debugging. *)
module StringMap : Map.S with type key = Ast.id

(* The environment type, parameterized on the types of values and functions.
 * The representation is exposed for ease of debugging. *)
type ('a, 'b) t = 
  {
    locals    : 'a StringMap.t;
    globals   : 'a StringMap.t;
    functions : 'b StringMap.t
  }

(* Look up a variable by name, first in the global and then in the local scope.
 * Throws an Error.Imp_err if not found. *)
val lookup_var : ('a, 'b) t -> Loc.loc -> Ast.id -> 'a

(* Look up a function by name.  Throws an Error.Imp_err if not found. *)
val lookup_fun : ('a, 'b) t -> Loc.loc -> Ast.id -> 'b

(* Bind the given global variable to the given value. *)
val bind_global : ('a, 'b) t -> Ast.id -> 'a -> ('a, 'b) t

(* Take the locals from the first environment and the globals from the second,
 * and combine them into a new environment.  This operation is necessary to
 * implement function calls: we need to remember updates to the global
 * environment from the function call, while discarding local variables. *)
val combine : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

(* Set an existing variable's value.  Throws an Error.Imp_err if not found. *)
val set : ('a, 'b) t -> Loc.loc -> Ast.id -> 'a -> ('a, 'b) t

(* Replace all local bindings with the given list of bindings. *)
val bind_locals : ('a, 'b) t -> (Ast.id * 'a) list -> ('a, 'b) t

(* Bind the given function. *)
val bind_fun : ('a, 'b) t -> Ast.id -> 'b -> ('a, 'b) t

(* An empty environment. *)
val empty : ('a, 'b) t

