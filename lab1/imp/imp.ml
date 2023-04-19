(* The Imp interpreter. *)

open Sexprlib
open Sexpr

(*
 * Useful modules.
 *
 * Note that since the String module contains a "compare" function
 * which is compatible with OrderedType, we can use the entire module
 * as an input to the Map.Make and Set.Make functors.
 *)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(*
 * Exceptions used in this module.
 *)

(* An invalid form was encountered during syntax analysis. *)
exception Syntax_err of Loc.loc * string

(* A call does not match the function definition (wrong number of arguments). *)
exception Call_err of Loc.loc * string * int * int

(* A variable or function could not be found in the current environment. *)
exception Name_err of Loc.loc * string

(* A `use`d file could not be open.
 * Contains the source location of the file name, the name of the file,
 * and the message given by the runtime. *)
exception Use_err of Loc.loc * string * string

(* The same as Use_err, but for files loaded from the command-line. *)
exception Load_err of string * string

(* A unit test appeared outside of a "use"d file. *)
exception Unit_test_err of Loc.loc

(* A runtime error. *)
exception Runtime_err of Loc.loc * string

(*
 * The abstract syntax tree.
 *)

(* Type of identifiers. *)
type id = string

(* Imp expressions. *)
type exp =
  | Literal of Loc.loc * int
  | Var     of Loc.loc * id
  | Set     of Loc.loc * id * exp
  | If      of Loc.loc * exp * exp * exp
  | While   of Loc.loc * exp * exp
  | Begin   of Loc.loc * exp list
  | Call    of Loc.loc * id * exp list

(* Imp top-level forms. *)
type def =
  | Val         of Loc.loc * id * exp
  | Define      of Loc.loc * id * id list * exp
  | Use         of Loc.loc * string
  | CheckExpect of Loc.loc * exp * exp
  | CheckError  of Loc.loc * exp
  | Exp         of Loc.loc * exp

(* Extract location from exp value. *)
let loc_of_exp = function
  | Literal (l, _)       -> l
  | Var     (l, _)       -> l
  | Set     (l, _, _)    -> l
  | If      (l, _, _, _) -> l
  | While   (l, _, _)    -> l
  | Begin   (l, _)       -> l
  | Call    (l, _, _)    -> l

(* Extract location from def value. *)
let loc_of_def = function
  | Val         (l, _, _)       -> l
  | Define      (l, _, _, _) -> l
  | Use         (l, _)       -> l
  | CheckExpect (l, _, _)    -> l
  | CheckError  (l, _)       -> l
  | Exp         (l, _)       -> l

(*
 * Values and environments.
 *)

type imp_func =
  (* Primitive Imp functions.
   *
   * The first argument is the source location of the call.
   * The second argument is the list of values the function takes.
   * The function returns the integer return value.
   * Primitive functions never alter the environment. *)
  | PrimFunction of (Loc.loc -> int list -> int)

  (* User-defined imp functions.
   *
   * The first field is the list of formal parameters.
   * The second field is the body of the function. *)
  | UserFunction of id list * exp

(* Environments.
 *
 * An environment consists of a global function environment, a global variable
 * environment, and a local variable environment (for now, just function
 * arguments).
 *
 * Note that the environment structure is immutable.  We do it this way so that
 * the implementation will closely match the operational semantics, which
 * describe the execution in terms of explicit relations between environments. *)
type environment =
  {
    global_funcs : imp_func StringMap.t;   (* "phi" in operational semantics *)
    global_vars  : int StringMap.t;        (* "xi"  in operational semantics *)
    local_vars   : int StringMap.t         (* "rho" in operational semantics *)
  }

(* Define a unary function from int to int. *)
let prim_unary name f = PrimFunction
  (fun l args ->
     match args with
       | [arg] -> f arg
       | _ -> raise (Call_err (l, name, 1, List.length args)))

(* Define a binary function from two ints to an int. *)
let prim_binary name f = PrimFunction
  (fun l args ->
     match args with
       | [arg1; arg2] -> f arg1 arg2
       | _ -> raise (Call_err (l, name, 2, List.length args)))

(* Division is special because it can raise a divide-by-zero exception. *)
let prim_div = PrimFunction
  (fun l args ->
     match args with
       | [arg1; arg2] ->
         begin
           try
             arg1 / arg2
           with Division_by_zero ->
             raise (Runtime_err (l, "division by zero"))
         end
       | _ -> raise (Call_err (l, "/", 2, List.length args)))

(* Define a comparison function.  Note that 1 is "true" and 0 is "false". *)
let prim_comp name f =
  prim_binary name (fun x y -> if f x y then 1 else 0)

(* An association list of primitive functions (the initial basis). *)
let prims =
  [ ("+",       prim_binary "+"  ( + ));
    ("-",       prim_binary "-"  ( - ));
    ("*",       prim_binary "*"  ( * ));
    ("/",       prim_div);
    ("<",       prim_comp   "<"  ( < ));
    (">",       prim_comp   ">"  ( > ));
    ("=",       prim_comp   "="  ( = ));
    ("!=",      prim_comp   "!=" ( <> ));
    ("print",   prim_unary  "print"
       (fun i -> print_int i; i));
    ("printc",  prim_unary  "printc"
       (fun i -> Printf.printf "%c" (Char.chr i); i));
    ("println", prim_unary  "println"
       (fun i -> Printf.printf "%d\n" i; i));
  ]

(* Converting association lists to StringMaps. *)
let assoc_list_to_map l =
  List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty l

(* The initial environment (empty, except for the initial basis). *)
let init_environment =
  {
    global_funcs = assoc_list_to_map prims;
    global_vars  = StringMap.empty;
    local_vars   = StringMap.empty
  }

(* Keywords are names that can't be used as identifiers. *)
let keywords = 
  ["val"; "define"; "use";
   "check-expect"; "check-error";
   "set"; "if"; "while"; "begin"]

(*
 * Syntax analysis.
 *)

(* We don't need an actual parser because we'll just use the s-expression
 * parser, but we do need to convert those s-expressions into the Imp AST. *)

(* Syntax analysis for expressions. *)
let rec parse_expr = function
  | Int (l, i) -> Literal (l, i)

  | Id (l, name) ->
    if List.mem name keywords
    then raise (Syntax_err (l, "keywords can't be variable names"))
    else Var (l, name)

  (* Special forms. *)

  | List (l, [Id (_, "set"); Id (_, name); e]) ->
    if List.mem name keywords
    then raise (Syntax_err (l, "keywords can't be variable names"))
    else Set (l, name, parse_expr e)
  | List (l, Id (_, "set") :: _) -> raise (Syntax_err (l, "invalid \"set\""))

  | List (l, [Id (_, "if"); e1; e2; e3]) ->
    If (l, parse_expr e1, parse_expr e2, parse_expr e3)
  | List (l, Id (_, "if") :: _) -> raise (Syntax_err (l, "invalid \"if\""))

  | List (l, [Id (_, "while"); e1; e2]) ->
    While (l, parse_expr e1, parse_expr e2)
  | List (l, Id (_, "while") :: _) -> raise (Syntax_err (l, "invalid \"while\""))

  | List (l, Id (_, "begin") :: es) -> Begin (l, List.map parse_expr es)
    (* No error cases exist for "begin". *)

  (* Function calls. *)

  | List (l, Id (_, fname) :: es) ->
    if List.mem fname keywords then
      raise (Syntax_err (l, "keywords can't be function names"))
    else
      Call (l, fname, List.map parse_expr es)

  (* Anything else is an error. *)

  | List (l, _) -> raise (Syntax_err (l, "unrecognized form"))

(* Syntax analysis for definitions. *)
let parse_def = function
  | List (l, [Id (_, "val"); Id (ln, name); init]) ->
    if List.mem name keywords then
      raise (Syntax_err (ln, "keywords can't be variable names"))
    else
      Val (l, name, parse_expr init)
  | List (l, Id (_, "val") :: _) ->
    raise (Syntax_err (l, "invalid \"val\""))

  | List (l, [Id (_, "define"); Id (lf, fname); List (_, args); body]) ->
    if List.mem fname keywords
    then raise (Syntax_err (lf, "keywords can't be function names"))
    else
      (* Convert the formal parameters to strings. *)
      let to_argname =
        let err l name =
          raise (Syntax_err (l, name ^ " used as formal parameter"))
        in
          (* Formal parameters must be identifiers. *)
          function
            | Id   (l, name) ->
              if List.mem name keywords then
                err l "keyword"
              else
                name
            | List (l, _) -> err l "list"
            | Int  (l, _) -> err l "int"
      in
      let args' = List.map to_argname args in
        (* Check that the formal parameters are unique. *)
        if StringSet.cardinal (StringSet.of_list args') = List.length args'
        then
          Define (l, fname, args', parse_expr body)
        else
          raise (Syntax_err (l, "non-unique formal parameter names"))

  | List (l, Id (_, "define") :: _) ->
    raise (Syntax_err (l, "invalid \"define\""))

  | List (l, [Id (_, "use"); Id (_, filename)]) -> Use (l, filename)
  | List (l, Id (_, "use") :: _) -> raise (Syntax_err (l, "invalid \"use\""))

  | List (l, [Id (_, "check-expect"); to_check; result]) ->
    CheckExpect (l, parse_expr to_check, parse_expr result)
  | List (l, Id (_, "check-expect") :: _) ->
    raise (Syntax_err (l, "invalid \"check-expect\""))

  | List (l, [Id (_, "check-error"); to_check]) ->
    CheckError (l, parse_expr to_check)
  | List (l, Id (_, "check-error") :: _) ->
    raise (Syntax_err (l, "invalid \"check-error\""))

  | other ->
    let exp = parse_expr other in
    let l = loc_of_exp exp in
      Exp (l, exp)

(*
 * The evaluator.
 *)

(* The result from evaluating a definition.
 * This is used by the REPL for printing out the result of
 * evaluation.  Other than that, it has no significance. *)
type result =
  | Value    of int
  | Function of id
  | Variable of id * int
  | Use      of string

(* Unit tests. *)
type utest =
  | CheckExpectTest of exp * exp
  | CheckErrorTest  of exp

(* The recursive expression evaluator.  Updates the environment to keep track of
 * changes to variables, and returns an integer value. *)
let rec eval_expr env exp =
  let module SM = StringMap in   (* local alias *)
    match exp with
      | Literal (_, i) -> (env, i)

      | Var (l, s) ->
        begin
          (* First, try to find a corresponding local variable. *)
          try
            (env, SM.find s env.local_vars)
          (* If that fails, try the global environment. *)
          with Not_found ->
            try
              (env, SM.find s env.global_vars)
            (* If that fails, raise an exception. *)
            with Not_found ->
              raise (Name_err (l, s))
        end

      | Set (l, s, e) ->
        let (env', v) = eval_expr env e in
          (* Update local_vars if there is a corresponding local variable. *)
          if SM.mem s env.local_vars then
            let local_vars = SM.add s v env'.local_vars in
              ({ env' with local_vars }, v)
          else if SM.mem s env.global_vars then
            (* Otherwise, try global_vars. *)
            let global_vars = SM.add s v env'.global_vars in
              ({ env' with global_vars }, v)
          else
            (* Otherwise, it's an error. *)
            raise (Name_err (l, s))

      | If (_, cond, t, e) ->
        (* First, evaluate the condition. *)
        let (env', v) = eval_expr env cond in
          (* Note that we only ever execute one of the branches. *)
          if v <> 0 then
            eval_expr env' t
          else
            eval_expr env' e

      | While (l, cond, body) ->
        let (env', v) = eval_expr env cond in
          if v = 0 then  (* we're done *)
            (env', 0)
          else  (* one more time *)
            let (env'', _) = eval_expr env' body in
              eval_expr env'' (While (l, cond, body))

      (* An empty "begin" arbitrarily returns 0. *)
      | Begin (_, []) -> (env, 0)
      (* A singleton `begin` is equivalent to its constituent expression. *)
      | Begin (_, [e]) -> eval_expr env e
      (* Any other begin is just the first expression, followed by the Begin
       * containing the rest. *)
      | Begin (l, e :: es) ->
        let (env', _) = eval_expr env e in
          eval_expr env' (Begin (l, es))

      | Call (l, fn, args) ->
        (* First, look up the function definition. *)
        let f =
          try
            SM.find fn env.global_funcs
          with Not_found ->
            raise (Name_err (l, fn))
        in

        (* Then, evaluate all of the arguments.
         * All environment changes persist in the calling environments. *)
        let (env', args') =
          List.fold_left
            (fun (e, ars) a ->
               let (e', a') = eval_expr e a in
                 (e', a' :: ars))
            (env, []) args
        in

        (* We reversed the argument list, so reverse it back. *)
        let args'' = List.rev args' in

        (* Finally, run the function. *)
        let (env'', x) = eval_function l fn f args'' env' in

          (* Get rid of the local variables.
           * Only changes to global variables persist. *)
          ({ env' with global_vars = env''.global_vars }, x)

(* Evaluate an imp_func given its actual parameters, the source location of the
 * call, and the environment of the caller. *)
and eval_function loc name f actuals env =
  match f with
    | PrimFunction func -> (env, func loc actuals)
    | UserFunction (formals, body) ->
      let arg_map =
        try
          assoc_list_to_map (List.combine formals actuals)
        with Invalid_argument _ ->
          (* # formal arguments != # actual arguments *)
          raise (Call_err (loc, name, List.length formals, List.length actuals))
      in
      (* Add those to the calling environment. *)
      let env' = { env with local_vars = arg_map } in
        (* Evaluate the body of the function. *)
        eval_expr env' body

(* Pretty-print a location to stderr. *)
let print_loc l =
  begin
    Printf.eprintf "    at %s\n" (Loc.string_of_loc_short l);
    flush stderr;
  end

(* Run a unit test.
 * Return whether the test passed.
 * Prints a message otherwise. *)
let eval_test env = function
  | CheckExpectTest (to_check, result) ->
    let (_, actual)   = eval_expr env to_check in
    let (_, expected) = eval_expr env result in
      if actual <> expected
      then
        begin
          Printf.eprintf "check-expect failed: got %d but expected %d\n"
            actual expected;
          flush stderr;
          print_loc (loc_of_exp to_check);
          false
        end
      else true
  | CheckErrorTest to_check ->
    try
      ignore (eval_expr env to_check);
      Printf.eprintf "check-error failed: evaluated with no error\n";
      flush stderr;
      print_loc (loc_of_exp to_check);
      false
    with
      | _ -> true   (* any exception works *)

(* The evaluator for top-level forms.
 * Returns the updated environment and a result.
 * The result is for printing in the REPL only. *)
let rec eval_def env def =
  let module SM = StringMap in
    match def with
      | Val (_, name, e) ->
        let env', v = eval_expr env e in
          ({env' with global_vars = SM.add name v env'.global_vars},
           Variable (name, v))
      | Define (_, fname, formals, body) ->
        let func = UserFunction (formals, body) in
          ({env with global_funcs = SM.add fname func env.global_funcs},
           Function fname)
      | Use (l, filename) -> load_file (Some l) filename env
      | CheckExpect (l, _, _)
      | CheckError  (l, _) -> raise (Unit_test_err l)
      | Exp (_, e) ->
        let env', v = eval_expr env e in
          ({env' with global_vars = SM.add "_" v env'.global_vars},
           Value v)

and load_file l filename env =
  try
    (* Read in and parse the file. *)
    let sexprs = Parser.parse_file filename in

    (* Helper which folds over the list of definitions in the file,
     * keeping track of modifications to both the list of unit tests and
     * the environment. *)
    let folder (env, unit_tests) expr =
      eval_def_in_use env unit_tests (parse_def expr) in

    (* Finally, actually evaluate the contents of the file. *)
    let env, unit_tests = List.fold_left folder (env, []) sexprs in

    (* Helper which folds over the unit tests, printing a message if one
     * fails and keeping track of how many pass (in `count`). *)
    let test_folder test count =
      if eval_test env test then count + 1 else count
    in

    (* Fold that over unit_tests.  Has to be a right fold because we
     * keep unit_tests in reverse order of definition but we want to
     * run the tests in the order of definition. *)
    let count_passed = List.fold_right test_folder unit_tests 0 in
      Printf.printf "%d of %d unit tests passed\n"
        count_passed (List.length unit_tests);
      flush stdout;
      (env, Use filename)
  with
    | Sys_error msg ->
      begin
        match l with
          | None ->
            (* Loading a file from a command-line argument to the interpreter. *)
            raise (Load_err (filename, msg))
          | Some l' ->
            (* Loading a file from a "use" *)
            raise (Use_err (l', filename, msg))
      end

    | Lexer.Lexer_error (loc, msg) ->
      begin
        match l with
          | None -> raise (Load_err (filename, msg))
          | Some _ -> raise (Use_err (loc, filename, msg))
      end

    | Parser.Parse_error (loc, msg) ->
      begin
        match l with
          | None -> raise (Load_err (filename, msg))
          | Some _ -> raise (Use_err (loc, filename, msg))
      end

    | Parser.Parse_incomplete ->
      begin
        match l with
          | None -> raise (Load_err (filename, "incomplete parse"))
          | Some loc -> raise (Use_err (loc, filename, "incomplete parse"))
      end

    | err ->
      begin
        handle_error err;
        match l with
          | None -> raise (Load_err (filename, "error on load"))
          | Some loc -> raise (Use_err (loc, filename, "error on use"))
      end


(* Just like `eval_def`, but additionally keeps track of a list of unit tests
 * and can therefore handle unit test forms. *)
and eval_def_in_use env tests def =
  match def with
    | CheckExpect (_, to_check, result) ->
      let test = CheckExpectTest (to_check, result) in
        (env, test :: tests)
    | CheckError (_, to_check) ->
      let test = CheckErrorTest to_check in
        (env, test :: tests)
    | _ ->
      let env, _ = eval_def env def in
        (* result is ignored *)
        (env, tests)

(* Handle an expected error.  Re-raise an unexpected one. *)
and handle_error err =
  begin
    begin
      match err with
        | Name_err (loc, v) ->
          Printf.eprintf "Unknown name: %s\n" v;
          print_loc loc
        | Call_err (loc, name, expected, found) ->
          Printf.eprintf "Wrong number of arguments to function [%s]: " name;
          Printf.eprintf "expected %d; found %d\n" expected found;
          print_loc loc
        | Syntax_err (loc, msg) ->
          Printf.eprintf "Syntax error: %s\n" msg;
          print_loc loc
        | Use_err (loc, filename, msg) ->
          Printf.eprintf "Could not load file %s (%s).\n" filename msg;
          print_loc loc
        | Unit_test_err loc ->
          Printf.eprintf "Cannot run unit test in the REPL.\n";
          print_loc loc
        | Runtime_err (loc, msg) ->
          Printf.eprintf "Runtime error: %s\n" msg;
          print_loc loc
        | other -> raise other
    end;
    flush stderr
  end

(* Load the initial basis. *)
let load_initial_basis init_env s =
  let lexbuf = Lexing.from_string s in
  let sexprs = Parser.parse_many "<initial basis>" lexbuf in
  let folder env expr =
    let (env, _) = eval_def env (parse_def expr) in env
  in
    try
      List.fold_left folder init_env sexprs
    with err ->
      begin
        handle_error err;
        exit 1  (* An error in the initial basis is fatal. *)
      end

(* The REPL, in the form expected by Repl. *)
let rec repl_func env = function
  | [] -> env
  | x :: xs ->
    try
      let def = parse_def x in
      let env', v = eval_def env def in
        begin
          begin
            match v with
              | Variable (n, v) -> Printf.printf "variable %s = %d" n v
              | Value v -> print_int v
              | Function n -> Printf.printf "function %s defined" n
              | Use filename -> Printf.printf "loaded file: %s" filename
          end;
          print_char '\n';
          repl_func env' xs
        end
    with err ->
      begin
        handle_error err;
        env
      end

(* The initial basis (user-defined functions). *)
let initial_basis = "
(define and (b c) (if b c b))
(define or (b c) (if b b c))
(define not (b) (if b 0 1))
(define <= (x y) (not (> x y)))
(define >= (x y) (not (< x y)))
(define != (x y) (not (= x y)))
(define mod (m n) (- m (* n (/ m n))))
"

let () =
  match Sys.argv with
    | [| _ |] ->
      let repl = Repl.make_repl repl_func in
        begin
          try
            let env = load_initial_basis init_environment initial_basis in
              ignore (repl env)
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
          let env = load_initial_basis init_environment initial_basis in
            ignore (load_file None filename env)
        with
          Load_err (filename, msg) ->
            begin
              Printf.fprintf stderr "Could not load file %s (%s).\n%!" filename msg;
              exit 1
            end
      end

    | _ ->
      begin
        Printf.fprintf stderr "usage: ./imp [filename]\n";
        exit 1;
      end

