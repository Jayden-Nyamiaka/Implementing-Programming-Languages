(** Read-eval-print loops (REPLs). *)

(** make_repl: generate a read-eval-print loop (REPL).
 *
 *  arguments:
 *    eval_print_fn: 
 *      a function taking in an environment and a list of S-expressions,
 *      and returning a (possibly updated) environment
 *    env: the initial environment
 *
 *  return value:
 *    the resulting environment when the REPL exits.
 *)
val make_repl : ('a -> Sexpr.expr list -> 'a) -> 'a -> 'a
