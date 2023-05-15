open Printf
open Sexprlib

(* Error tags.  
 * They correspond to the possible errors documented in error.mli. *)
type uscheme_error_tag =
  | ContinueOutsideWhile
  | BreakOutsideWhile
  | CallError             of int * int
  | ContextStackOverflow
  | ContinuationCallError of int
  | InternalError         of string
  | NameError             of string
  | ReturnOutsideFunction
  | RuntimeError          of string
  | SyntaxError           of string
  | TypeError             of string * string
  | UncaughtThrow         of string
  | UnitTestError
  | UnspecifiedError      of string
  | UseError              of string * string

type uscheme_error_info = Loc.loc * uscheme_error_tag

exception UScheme_err of uscheme_error_info

(* Pretty-print a location to stderr. *)
let print_loc l = eprintf "    at %s\n%!" (Loc.string_of_loc_short l)

(* Print a uscheme error to stderr. *)
let print_err (loc, tag) = 
  begin
    begin
      match tag with
        | ContinueOutsideWhile
        | BreakOutsideWhile ->
          eprintf "Attempted to break outside of any while loop.\n"
        | CallError (expected, found) ->
          eprintf "Wrong number of arguments: ";
          eprintf "expected %d; found %d\n" expected found
        | ContextStackOverflow -> eprintf "Context stack overflow.\n"
        | ContinuationCallError found ->
          eprintf "Wrong number of arguments to continuation call: ";
          eprintf "expected 1; found %d\n" found
        | InternalError msg ->
          eprintf "Internal error: %s\n" msg
        | NameError v ->
          eprintf "Unknown name: %s\n" v
        | RuntimeError msg ->
          eprintf "Runtime error: %s\n" msg;
        | ReturnOutsideFunction ->
          eprintf "Attempted to return outside of any function.\n"
        | SyntaxError msg ->
          eprintf "Syntax error: %s\n" msg;
        | TypeError (expected, found) ->
          eprintf "Type error: expected %s; got %s.\n" expected found
        | UncaughtThrow e ->
          eprintf "Uncaught exception: %s.\n" e
        | UnitTestError ->
          eprintf "Cannot run unit test in the REPL.\n";
        | UnspecifiedError msg ->
          eprintf "Unspecified error: %s\n" msg
        | UseError (filename, msg) ->
          eprintf "Could not open file %s (%s).\n" filename msg
    end;
    print_loc loc;
  end

let uscheme_err l i = 
  raise (UScheme_err (l, i))

let continue_outside_while l = 
  uscheme_err l ContinueOutsideWhile

let break_outside_while l = 
  uscheme_err l BreakOutsideWhile

let call_err l ~expected ~found = 
  uscheme_err l (CallError (expected, found))

let context_stack_overflow l = 
  uscheme_err l ContextStackOverflow

let continuation_call_err l found = 
  uscheme_err l (ContinuationCallError found)

let internal_err l msg =
  uscheme_err l (InternalError msg)

let name_err l v = 
  uscheme_err l (NameError v)

let return_outside_func l = 
  uscheme_err l ReturnOutsideFunction

let runtime_err l msg = 
  uscheme_err l (RuntimeError msg)

let syntax_err l msg = 
  uscheme_err l (SyntaxError msg)

let type_err l ~expected ~found = 
  uscheme_err l (TypeError (expected, found))

let uncaught_throw l e = 
  uscheme_err l (UncaughtThrow e)

let unit_test_err l = 
  uscheme_err l UnitTestError

let unspecified_err l name = 
  uscheme_err l (UnspecifiedError name)

let use_err l ~filename ~msg = 
  uscheme_err l (UseError (filename, msg))

