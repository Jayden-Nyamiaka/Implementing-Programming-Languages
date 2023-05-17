(* The Typed Imp interpreter: evaluator *)

open Sexprlib

(* REPL result values. *)
type result =
  | Value    of Value.value * Typecheck.imp_type
  | Function of Ast.id * Typecheck.function_type
  | Variable of Ast.id * Value.value * Typecheck.imp_type
  | Use      of string

(* Given an environment and a definition, evaluate the definition and 
 * update the environment.  Also return a result, for the benefit of the
 * REPL (if there is one). *)
val eval_def : Basis.env -> Ast.def -> Basis.env * result

(* Read in and evaluate code from a lexbuf.
 * Return the updated environment plus the results of running the unit
 * tests (number of passes, number of total tests. *)
val use_lexbuf :
  Lexing.lexbuf -> string -> Basis.env -> Basis.env * int * int

(* Load in a file given a filename.
 * The loc option argument is None if the file is loaded as a command-line
 * argument, and Some if the file is loaded as a "use" form from inside
 * a program or from the REPL (and hence has a code location).
 * Return an updated environment and a value, like `eval_def`. *)
val load_file : string -> Loc.loc option -> Basis.env -> Basis.env * result
