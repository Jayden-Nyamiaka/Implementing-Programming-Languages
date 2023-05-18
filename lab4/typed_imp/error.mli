(* Interface for propagating and handling user errors. *)

open Sexprlib

(* Error tags. *)
type error_tag =
  | BugInTypeChecker of string
  | CallError        of int * int
  | NameError        of string
  | RuntimeError     of string
  | SyntaxError      of string
  | TypeError        of string * string
  | IndexError       of int
  | UnitTestError
  | UseError         of string * string

type error_info = Loc.loc * error_tag

exception Imp_err of error_info

(*
 * Printing error info to stderr.
 *)

val print_loc : Loc.loc -> unit
val print_err : error_info -> unit

(*
 * Functions for throwing Imp_errs.
 *)

(* The type checker failed to detect a type error.
 * The string argument is the string version of the type 
 * that was expected. *)
val bug_in_type_checker_err : Loc.loc -> string -> 'a

(* A function call had the wrong number of arguments. *)
val call_err : Loc.loc -> expected: int -> found: int -> 'a

(* The given name is not bound. *)
val name_err : Loc.loc -> string -> 'a

(* A runtime error (e.g. division by zero) occurred. *)
val runtime_err : Loc.loc -> string -> 'a

(* Syntax analysis failed for the given reason. *)
val syntax_err : Loc.loc -> string -> 'a

(* Expected something of type `expected`; got something of type `found`. *)
val type_err : Loc.loc -> expected: string -> found: string -> 'a

(* The given array index is out of bounds (bounds are [0, Len - 1]). *)
val index_err : Loc.loc -> int -> 'a

(* A unit test was entered interactively. *)
val unit_test_err : Loc.loc -> 'a

(* A `use` could not open the given file for the given reason. *)
val use_err : Loc.loc -> filename: string -> msg: string -> 'a

