(* The Typed Imp interpreter: entry point. *)

open Sexprlib

open Eval
open Ast

let progname = "typed_imp"

(* The REPL, in the form expected by Repl. *)
let rec repl_func env = function
  | [] -> env
  | x :: xs -> try
      let def = parse_def x in
      let env, v = eval_def env def in
        begin
          begin 
            match v with
              | Variable (n, v, ty) ->
                let ty_name = Typecheck.string_of_type ty in
                  Printf.printf "%s : %s = %s\n" 
                    n ty_name (Value.string_of_value v)
              | Value (v, ty) ->
                if not (ty = UnitType) then
                  Printf.printf "%s : %s\n" 
                    (Value.string_of_value v) (Typecheck.string_of_type ty)
              | Function (n, ty) ->
                Printf.printf "%s: %s\n%!" n (Typecheck.string_of_function_type ty)
              | Use filename -> Printf.printf "used file: %s\n" filename
          end;
          flush stdout;
          env
        end
    with Error.Imp_err info -> Error.print_err info;
      flush stderr;
      repl_func env xs

let load_initial_basis () =
  try
    let env = Basis.basis in
    let basis_buf = Lexing.from_string Basis.user_basis in
    let (env', _, _) = use_lexbuf basis_buf "<initial basis>" env in
      env'
  with 
    | Error.Imp_err err ->
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
          | Error.Imp_err err ->
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

