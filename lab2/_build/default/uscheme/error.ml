open Printf
open Sexprlib

(* Error tags.  
 * They correspond to the possible errors documented in error.mli. *)
type uscheme_error_tag =
  | CallError        of int * int
  | CallErrorRest    of int * int
  | InternalError    of string
  | NameError        of string
  | RuntimeError     of string
  | SyntaxError      of string
  | TypeError        of string * string
  | UnitTestError
  | UnspecifiedError of string
  | UseError         of string * string

type uscheme_error_info = Loc.loc * uscheme_error_tag

exception UScheme_err of uscheme_error_info

(* Pretty-print a location to stderr. *)
let print_loc l = eprintf "    at %s\n%!" (Loc.string_of_loc_short l)

(* Print a uscheme error to stderr. *)
let print_err (loc, tag) = 
  begin
    begin
      match tag with
        | CallError (expected, found)
        | CallErrorRest (expected, found) ->
          eprintf "Call error: wrong number of arguments: ";
          eprintf "expected %d; found %d\n" expected found
        | InternalError msg ->
          eprintf "Internal error: %s\n" msg
        | NameError msg ->
          eprintf "Unknown name: %s\n" msg
        | RuntimeError msg ->
          eprintf "Runtime error: %s\n" msg;
        | SyntaxError msg ->
          eprintf "Syntax error: %s\n" msg;
        | TypeError (expected, found) ->
          eprintf "Type error: expected value of type %s; got: %s\n" expected found
        | UnitTestError ->
          eprintf "Unit test error: cannot run unit test in the REPL.\n";
        | UnspecifiedError msg ->
          eprintf "Unspecified error: %s\n" msg
        | UseError (filename, msg) ->
          eprintf "Use error: could not open file %s (%s).\n" filename msg
    end;
    print_loc loc;
    flush stderr
  end

let uscheme_err l i = 
  raise (UScheme_err (l, i))

let call_err l ~expected ~found = 
  uscheme_err l (CallError (expected, found))

let call_err_rest l ~expected ~found = 
  uscheme_err l (CallErrorRest (expected, found))

let internal_err l msg =
  uscheme_err l (InternalError msg)

let name_err l name = 
  uscheme_err l (NameError name)

let runtime_err l msg = 
  uscheme_err l (RuntimeError msg)

let syntax_err l msg = 
  uscheme_err l (SyntaxError msg)

let type_err l ~expected ~found = 
  uscheme_err l (TypeError (expected, found))

let unit_test_err l = 
  uscheme_err l UnitTestError

let unspecified_err l name = 
  uscheme_err l (UnspecifiedError name)

let use_err l ~filename ~msg = 
  uscheme_err l (UseError (filename, msg))

