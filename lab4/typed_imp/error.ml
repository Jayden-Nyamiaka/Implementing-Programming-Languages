open Sexprlib

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

(* Pretty-print a location to stderr. *)
let print_loc l = Printf.eprintf "    at %s\n%!" (Loc.string_of_loc_short l)

(* Print an error to stderr. *)
let print_err (loc, tag) = 
  begin
    begin
      match tag with
        | BugInTypeChecker (expected) ->
          Printf.eprintf "Bug in type checker: expected type(s): %s\n" expected
        | CallError (expected, found) ->
          begin
            Printf.eprintf "Wrong number of arguments: ";
            Printf.eprintf "expected %d; found %d\n" expected found
          end
        | SyntaxError msg ->
          Printf.eprintf "Syntax error: %s\n" msg;
        | NameError v ->
          Printf.eprintf "Unknown name: %s\n" v
        | RuntimeError msg ->
          Printf.eprintf "Runtime error: %s\n" msg
        | TypeError (expected, found) ->
          Printf.eprintf "Type error: expected %s; got %s\n" expected found
        | IndexError i ->
          Printf.eprintf "Array index out of bounds: %d\n" i
        | UnitTestError ->
          Printf.eprintf "Cannot run unit test in the REPL\n";
        | UseError (filename, msg) ->
          Printf.eprintf "Could not open file %s (%s)\n" filename msg
    end;
    print_loc loc;
  end

let err l i = raise (Imp_err (l, i))

let bug_in_type_checker_err l expected =
  err l (BugInTypeChecker expected)

let name_err l v = 
  err l (NameError v)

let call_err l ~expected ~found = 
  err l (CallError (expected, found))

let runtime_err l i = 
  err l (RuntimeError i)

let syntax_err l msg = 
  err l (SyntaxError msg)

let type_err l ~expected ~found = 
  err l (TypeError (expected, found))

let index_err l idx = 
  err l (IndexError idx)

let unit_test_err l = 
  err l UnitTestError

let use_err l ~filename ~msg = 
  err l (UseError (filename, msg))

