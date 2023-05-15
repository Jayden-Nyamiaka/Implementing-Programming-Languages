(* The uScheme+ interpreter: entry point. *)

open Sexprlib

open Env
open Eval
open Ast

(* The REPL, in the form expected by Repl.make_repl. *)
let repl_func env sexprs = 
  let repl_print def v =
    (* Unit values don't print.
     * Top-level expressions just print the value.
     * Other expressions print the name and value. *)
    match (def, v) with
      | (_, UnitVal) -> ()
      | (Exp _, _) -> print_endline (string_of_value v)
      | (Val (_, name, _), v) -> 
        Printf.printf "val %s = %s\n%!" name (string_of_value v)
      | (Define (_, name, _, _), v) ->
        Printf.printf "val %s = %s\n%!" name (string_of_value v)
      | _ -> Error.internal_err (loc_of_def def) "invalid top-level expression"
  in
  let iter1 env sexpr =
    try
      let def = parse_def sexpr in
      let (env', v) = eval_def env def in
        begin
          repl_print def v;
          env'
        end
    with Error.UScheme_err e -> 
      begin
        Error.print_err e;
        env
      end
  in
    List.fold_left iter1 env sexprs

let progname = "uscheme+"

let load_initial_basis () =
  try
    let env = Basis.basis in
    let basis_buf = Lexing.from_string Basis.scheme_basis in
    let (env', _, _) = use_lexbuf basis_buf "<initial basis>" env in
      env'
  with 
    | Error.UScheme_err err ->
      begin
        Error.print_err err;
        exit 1
      end
    | Parser.Parse_incomplete ->
      begin
        Printf.eprintf 
          "Syntax error: initial basis contains incomplete forms; exiting.\n%!";
        exit 1
      end

let _ = 
  match Sys.argv with
    | [| _ |] ->
      let env = load_initial_basis () in
        begin
          try 
            ignore (Repl.make_repl repl_func env)
          with 
            End_of_file ->
              begin
                Printf.printf "  \n";  (* an ugly hack to make exiting look clean *)
                exit 0
              end
        end
    | [| _; filename |] ->
      begin
        try
          let env = load_initial_basis () in
            ignore (load_file filename None env)
        with
          | Error.UScheme_err err ->
            begin
              Error.print_err err;
              exit 1
            end
          | Parser.Parse_incomplete ->
            begin
              Printf.eprintf 
                "Syntax error: file [ %s ] contains incomplete forms; exiting.\n%!" 
                filename;
              exit 1
            end
      end
    | _ -> 
      begin
        Printf.eprintf "usage: %s [filename]\n%!" progname;
        exit 1;
      end

