(* The uScheme+ interpreter: evaluator *)

open Sexprlib

(** Given an environment and a definition, evaluate the definition and 
    update the environment.  Also return a value, for the benefit of the
    REPL (if there is one). *)
val eval_def : Env.env -> Ast.def -> Env.env * Env.value

(** Read in and evaluate code from a lexbuf.
    Return the updated environment plus the results of running the unit
    tests (number of passes, number of total tests. *)
val use_lexbuf : Lexing.lexbuf -> string -> Env.env -> Env.env * int * int

(** Load in a file given a filename.
   The loc option argument is None if the file is loaded as a command-line
   argument, and Some if the file is loaded as a "use" form from inside
   a program or from the REPL (and hence has a code location).
   Return an updated environment and a value, like `eval_def`. *)
val load_file : string -> Loc.loc option -> Env.env -> Env.env * Env.value

(** Set the frame stack tracing level.
    Return value:
      If argument is 0:
        return the maximum stack depth since tracing started.
      Otherwise return 0.  *)
val trace_frame_stack : int -> int

