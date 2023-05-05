open Sexprlib

open Env
open Ir
module A = Ast

(* Flag; if true, print out information about each test. *)
let verbose_tests = true

(* Helper functions. *)

let bind_all = List.fold_left (fun e (k, v) -> Env.bind e k v)


(* Unit tests. *)

type unit_test = 
  | CheckExpectTest of Loc.loc * expr * expr
  | CheckErrorTest  of Loc.loc * expr

(* 
 * The expression evaluator. 
 *)

let rec eval_expr (env : Env.env) (expr : Ir.expr) : Env.value =
  match expr with
    | Literal (_, Unit) -> UnitVal
    | Literal (_, Boolean b) -> BoolVal b
    | Literal (_, Integer i) -> IntVal i

    | Quote (_, e) -> Quote.value_of_sexpr e

    | Var (l, s) -> 
      begin 
        match Env.lookup l env s with
          (* Can't read an unspecified value. *)
          | Unspecified s -> 
            let msg = 
              Printf.sprintf "attempt to read from an unspecified value: %s" s
            in
            Error.unspecified_err l msg
          | other -> other
      end

    | Set (l, s, e) ->
      begin
        match Env.lookup l env s with
          (* Can't write over an unspecified value in a `set`. *)
          | Unspecified s -> 
            let msg = 
              Printf.sprintf "attempt to write to an unspecified value: %s" s
            in
            Error.unspecified_err l msg
          | _ -> 
            begin
              Env.set l env s (eval_expr env e);
              UnitVal
            end
      end

    | If (l, cond, t, e) ->
      if truthy l (eval_expr env cond) then
        eval_expr env t
      else
        eval_expr env e

    | While (l, cond, body) ->
      if truthy l (eval_expr env cond) then 
        begin
          ignore (eval_expr env body);
          eval_expr env expr
        end 
      else
        UnitVal

    | Begin (_, []) -> UnitVal
    | Begin (_, [x]) -> eval_expr env x
    | Begin (l, x :: xs) ->
      begin
        ignore (eval_expr env x);
        eval_expr env (Begin (l, xs))
      end

    | Let (_, bindings, body) ->
      (* First evaluate all bindings in the outer environment. *)
      let bindings' = 
        List.map (fun (name, expr) -> (name, eval_expr env expr)) bindings 
      in
      let env' = bind_all env bindings' in
        eval_expr env' body

    | LetRec (l, bindings, body) ->
      (* First, assign some placeholders. *)
      let placeholders = 
        List.map (fun (name, _) -> (name, Unspecified name)) bindings 
      in
      let env' = bind_all env placeholders in
      (* Compute the new values given the new environment. *)
      let vals =
        List.map (fun (name, e) -> (name, eval_expr env' e)) bindings 
      in
      (* Now, rebind the names. *)
      begin
        List.iter (fun (name, v) -> Env.set l env' name v) vals;
        eval_expr env' body
      end

    | Lambda (_, formals, rest, body) -> UserFuncVal (formals, rest, body, env)

    | Call (l, func, args) ->
      let actuals = List.map (eval_expr env) args in
      match eval_expr env func with
        | PrimFuncVal f -> f l actuals
        | UserFuncVal (formals, None, body, env') ->
          if List.length formals <> List.length actuals then
            Error.call_err l ~expected:(List.length formals)
              ~found:(List.length actuals)
          else
            let env'' = bind_all env' (List.combine formals actuals) in
              eval_expr env'' body
        | UserFuncVal (formals, Some rest, body, env') ->
          (* Matches the bindings correctly for functions with a rest arg *)
          let rec iter (forms : string list) (acts: Env.value list) 
                                      : (string * Env.value) list =
            begin
            match (forms, acts) with 
              | ([], []) -> (rest, NilVal) :: []
              | (_, []) -> Error.call_err_rest l 
                ~expected:(List.length formals) ~found:(List.length actuals)
              | ([], leftover) -> 
                let rec make_scheme_list lst =
                  if lst = [] then NilVal
                  else PairVal (ref (List.hd lst), 
                                ref (make_scheme_list (List.tl lst)))
                in (rest, make_scheme_list leftover) :: []
              | (fh :: ft, ah :: at) -> (fh, ah) :: (iter ft at)
            end
          in 
          let bindings = iter formals actuals in
          let env'' = bind_all env' bindings in
            eval_expr env'' body
        | other -> Error.type_err l ~expected:"function" ~found:(string_of_value other)

(* Run a unit test. Return whether the test passed.
   Prints a message otherwise.
   If `verbose_tests` is `true`, print information for passing tests too. *)
let eval_test env = function
  | CheckExpectTest (loc, to_check, result) ->
    let actual = eval_expr env to_check in
    let expected = eval_expr env result in
      if actual <> expected then 
        begin
          Printf.eprintf "check-expect failed: got %s but expected %s\n%!"
            (string_of_value actual) (string_of_value expected);
          Error.print_loc loc;
          false
        end 
      else
        begin
          if verbose_tests then
            Printf.printf "check-expect test passed (%s)\n%!"
              (Loc.string_of_loc_short loc);
          true
        end
  | CheckErrorTest (loc, to_check) ->
    try
      begin
        ignore (eval_expr env to_check);
        Printf.eprintf "check-error failed: evaluated with no error\n%!";
        Error.print_loc loc;
        false
      end
    with Error.UScheme_err _ -> true

(* The evaluator for top-level forms. 
 * Returns the (possibly updated and extended) environment,
 * as well as a value used for printing in the REPL. *)
let rec eval_def env def =
  let make_binding envir name = 
    if Env.mem name envir
      then envir
      else Env.bind envir name (Unspecified name)
  in
  match def with
    (* Val semantics: If the name is already in the environment, 
     * just overwrite the contents of the existing binding.
     * Otherwise, extend the environment with a new binding. 
     * Bindings start off unspecified so that we can define
     * top-level recursive functions. *)
    | Val (l, name, e) ->
      let env' = make_binding env name in
      let v = eval_expr env' e in
        begin
          Env.set l env' name v;
          (env', v)
        end

    | ValRec (l, bindings) ->
      if bindings = [] then Error.syntax_err l (Printf.sprintf 
        "valrec statements require at least one definition") else
      let env' = 
        List.fold_left (fun en (n, _) -> make_binding en n) env bindings in
      let values = List.map (fun (_, e) -> eval_expr env' e) bindings in
      let rec iter binds vals =
        match (binds, vals) with
          | ([], _)
          | (_, []) -> NilVal
          | ( (n, _) :: bt, v :: vt) ->
            begin
              Env.set l env' n v;
              PairVal (ref v, ref (iter bt vt) )
            end
      in (env', iter bindings values)

    | Use (l, filename) -> 
      begin
        try
          load_file filename (Some l) env
        with Parser.Parse_incomplete ->
          Error.syntax_err l 
            (Printf.sprintf 
               "incomplete form at end of file: %s" filename)
      end

    | CheckExpect (l, _, _)
    | CheckError  (l, _) -> Error.unit_test_err l

(* Just like `eval_def`, but additionally keeps track of a list of unit tests
 * and can therefore handle unit test forms. 
 * Returns a (possibly updated/extended) environment and the list of tests. *)
and eval_def_in_use env tests def = 
  match def with
    | CheckExpect (l, to_check, result) ->
      let test = CheckExpectTest (l, to_check, result) in
        (env, test :: tests)
    | CheckError (l, to_check) ->
      let test = CheckErrorTest (l, to_check) in
        (env, test :: tests)
    | _ -> 
      let (env', _) = eval_def env def in
        (env', tests)

(* Parse and run the contents of a lexbuf in the current environment.
 *
 * Returns (new_env, num_tests_passed, num_total_tests).
 *)
and use_lexbuf lexbuf filename env =
  (* Parse the file. *)
  let sexprs = Parser.parse_many filename lexbuf in

  (* Helper which folds over the list of definitions in the file,
   * keeping track of modifications to both the list of unit tests and
   * the environment. *)
  let folder (env, unit_tests) expr = 
    eval_def_in_use env unit_tests (ir_of_ast_def (A.parse_def expr))
  in
  (* Finally, actually evaluate the contents of the file. *)
  let (env', unit_tests) = List.fold_left folder (env, []) sexprs in

  (* Helper which folds over the unit tests, printing a message if one
   * fails and keeping track of how many pass (in `count`). *)
  let test_folder test count =
    if eval_test env' test then count + 1 else count
  in

  (* Fold that over unit_tests.  Has to be a right fold because we
   * keep unit_tests in reverse order of definition but we want to
   * run the tests in the order of definition. *)
  let count_passed = List.fold_right test_folder unit_tests 0 in
    (env', count_passed, List.length unit_tests)

and load_file filename loc env =
  (* Read in and parse the file. *)
  let channel = 
    try open_in filename
    with Sys_error msg -> 
      begin
        match loc with
          | Some l -> Error.use_err l ~filename ~msg
          | None ->   (* filename given as command-line argument *)
            begin
              Printf.eprintf "file not found: %s\n%!" filename;
              exit 1
            end
      end
  in
  let lexbuf = Lexing.from_channel channel in
  let (env', count_passed, count_tests) = 
    use_lexbuf lexbuf filename env 
  in
    begin
      close_in channel;
      Printf.printf "%d of %d unit tests passed\n%!" count_passed count_tests;
      (env', UnitVal)
    end

