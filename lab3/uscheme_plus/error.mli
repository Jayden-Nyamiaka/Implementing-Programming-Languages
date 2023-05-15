(* Propagating and handling UScheme user errors. *)

open Sexprlib

type uscheme_error_info
exception UScheme_err of uscheme_error_info

(*
 * Printing error info to stderr.
 *)

val print_loc : Loc.loc -> unit

val print_err : uscheme_error_info -> unit

(*
 * Functions for throwing UScheme_errs.
 *)

(* A `continue` was encountered outside a `while` loop. *)
val continue_outside_while : Loc.loc -> 'a

(* A `break` was encountered outside a `while` loop. *)
val break_outside_while : Loc.loc -> 'a

(* A function call had the wrong number of arguments. *)
val call_err : Loc.loc -> expected: int -> found: int -> 'a

(* The context stack overflowed. *)
val context_stack_overflow : Loc.loc -> 'a

(* A continuation call had the wrong number of arguments. *)
val continuation_call_err : Loc.loc -> int -> 'a

(* Internal interpreter error. *)
val internal_err : Loc.loc -> string -> 'a

(* The given name is not bound. *)
val name_err: Loc.loc -> string -> 'a

(* `return` attempted outside a function. *)
val return_outside_func : Loc.loc -> 'a

(* A runtime error e.g. division by zero. *)
val runtime_err : Loc.loc -> string -> 'a

(* Syntax analysis failed for the given reason. *)
val syntax_err : Loc.loc -> string -> 'a

(* Expected something like `expected`; got something of type `found`. *)
val type_err : Loc.loc -> expected: string -> found: string -> 'a

(* Thrown exception has no `try/catch` to catch it.  Takes a rendering of the
 * exception. *)
val uncaught_throw : Loc.loc -> string -> 'a

(* A unit test was entered interactively. *)
val unit_test_err : Loc.loc -> 'a

(* Invalid use of an unspecified value. *)
val unspecified_err : Loc.loc -> string -> 'a

(* A `use` could not open the given file for the given reason. *)
val use_err : Loc.loc -> filename: string -> msg: string -> 'a

