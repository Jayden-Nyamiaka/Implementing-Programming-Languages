(*
 * Values.
 *
 * Contains datatypes for representing values, functions, and value environments
 * in Imp.
 *)

open Sexprlib

type value = 
  | Unit
  | Bool  of bool
  | Int   of int
  | Array of value array


(* Imp functions. *)
type func = 
  (* Primitive Imp functions.
   *
   * The first argument is the source location of the call,
   * the second is the values the function takes,
   * and the third is the environment in which the function is evaluated.
   * It returns the new global value environment and the return value.
   *)
  | PrimFunction of (Loc.loc -> value list -> env -> env * value)
  (* User-defined imp functions.
   *
   * The first argument is the list of formal parameters, 
   * the second is the list of local variables, and the third is the body.
   *
   * Note that a function doesn't capture an evironment--it gets evaluated in
   * the caller's environment.
   *)
  | UserFunction of Ast.id list * (Ast.id * value) list * Ast.exp

(* Value environments. *)
and env = (value, func) Env.t

val string_of_value : value -> string
val equal_values : Loc.loc -> value -> value -> bool
