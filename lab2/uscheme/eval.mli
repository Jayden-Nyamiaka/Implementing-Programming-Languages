(* The evaluator. *)

open Sexprlib

(** Evaluate a top-level form.  
    Return an updated environment and a value.  
    The value is only used for printing feedback in a REPL; 
    it's not part of the operational semantics. *)
val eval_def : Env.env -> Ir.def -> Env.env * Env.value

(** Read and evaluate forms from a file.
    The `lexbuf` is a lexing buffer.
    The `string` is the filename.
    The first `Env.env` is the initial environment.
    The second `Env.env` is the final environment.
    The `int`s are the number of tests passed and the 
      total number of tests, respectively. *)
val use_lexbuf : Lexing.lexbuf -> string -> Env.env -> Env.env * int * int

(** Load a file and evaluate its contents. 
    The `string` is the filename.  
    The `loc option` is the location of a `use` form in the source
      if this was called as the result of a `use`, or `None` if not.
    The first `Env.env` is the initial environment.
    The second `Env.env` is the final environment.
    The `Env.value` is the returned value (`UnitVal`) which is only used
      if this was called as the result of a `use` form. *)
val load_file : string -> Loc.loc option -> Env.env -> Env.env * Env.value

