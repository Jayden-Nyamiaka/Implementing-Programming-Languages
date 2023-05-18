open Sexprlib

open Basis
open Ast

(*
 * The evaluator.
 *)


(* The result from evaluating a definition. *)
type result = 
  | Value    of Value.value * Typecheck.imp_type
  | Function of id * Typecheck.function_type
  | Variable of id * Value.value * Typecheck.imp_type
  | Use      of string


(* Unit tests. *)
type unit_test = 
  | CheckExpectTest    of Loc.loc * exp * exp
  | CheckErrorTest     of Loc.loc * exp
  | CheckTypeTest      of Loc.loc * exp * imp_type
  | CheckTypeErrorTest of Loc.loc * exp
  | CheckFunTypeTest   of Loc.loc * id  * function_type

(* These functions extract the value (int and array) from the type. *)
let to_int l = function
  | Value.Int i -> i
  | _ -> Error.bug_in_type_checker_err l "int"
let to_array l = function
  | Value.Array a -> a
  | _ -> Error.bug_in_type_checker_err l "array"

(* Returns the default variable for each type 
 * used in initializing local variables. *)
let rec default_value_for_type = function
  | UnitType -> Value.Unit
  | BoolType -> Value.Bool false
  | IntType -> Value.Int 0
  | ArrayType base -> Value.Array (Array.make 0 (default_value_for_type base))

(* The recursive expression evaluator.  Updates the environment to keep track of
 * changes to variables, and returns an integer value. *)
let rec eval_expr env exp =
  let rec deepcopy v = 
    match v with 
    | Value.Array ar -> 
      Value.Array (Array.init (Array.length ar) (fun i -> deepcopy ar.(i)))
    | _ -> v
  in
  match exp with
    | Literal (_, UnitLit)   -> (env, Value.Unit)
    | Literal (_, BoolLit b) -> (env, Value.Bool b)
    | Literal (_, IntLit i)  -> (env, Value.Int i)

    | Var (l, s) -> (env, Env.lookup_var env l s)

    | Set (l, s, e) ->
      let (env', v) = eval_expr env e in
        (Env.set env' l s v, Value.Unit)

    | If (_, test, t, e) ->
      (* First, evaluate the test expression. *)
      let (env', v) = eval_expr env test in
        if v <> Value.Bool false then
          eval_expr env' t
        else
          eval_expr env' e

    | While (l, test, body) ->
      let (env', v) = eval_expr env test in
        if v = Value.Bool false then
          (env', Value.Unit)
        else
          let (env'', _) = eval_expr env' body in
          eval_expr env'' (While (l, test, body))

    (* An empty "begin" returns Unit. *)
    | Begin (_, []) -> (env, Value.Unit)
    (* A singleton `begin` is equivalent to its constituent expression. *)
    | Begin (_, [e]) -> eval_expr env e
    (* Any other begin is just the first expression, followed by the Begin
     * containing the rest. *)
    | Begin (l, e::es) -> 
      let (env', _) = eval_expr env e in
        eval_expr env' (Begin (l, es))

    | Call (l, fn, args) ->
      (* First, look up the function definition. *)
      let f = Env.lookup_fun env l fn in

      (* Then, evaluate all of the arguments. *)
      let (env, args) = List.fold_left 
        (fun (env, args) arg -> 
          let env, arg = 
            eval_expr env arg 
          in 
            (env, arg :: args)) 
        (env, []) args 
      in

      (* Oops, we reversed them. *)
      let args = List.rev args in

      (* Finally, run the function. *)
      let (call_env, x) = eval_function f l args env in

        (* Apply global updates from the function call. *)
        (Env.combine env call_env, x)

    | Print (_, e) ->
      let (env', v) = eval_expr env e in
        begin
          Printf.printf "%s" (Value.string_of_value v);
          (env', Value.Unit)
        end

    | Println (_, e) ->
      let (env', v) = eval_expr env e in
        begin
          Printf.printf "%s\n" (Value.string_of_value v);
          (env', Value.Unit)
        end

    | Eq (l, e1, e2) ->
      let (env', v1) = eval_expr env e1 in
      let (env'', v2) = eval_expr env' e2 in
        (env'', Value.Bool (Value.equal_values l v1 v2))

    | Neq (l, e1, e2) ->
      let (env', v1) = eval_expr env e1 in
      let (env'', v2) = eval_expr env' e2 in
        (env'', Value.Bool (not (Value.equal_values l v1 v2)))
    
    | ArrayMake (l, e1, e2) ->
      let (env', len) = eval_expr env e1 in
      let (env'', v) = eval_expr env' e2 in
        (env'', Value.Array (Array.init (to_int l len) (fun _ -> deepcopy v)))

    | ArrayAt (l, e1, e2) ->
      let (env', ar) = eval_expr env e1 in
      let (env'', idx) = eval_expr env' e2 in
      let v_idx = to_int l idx in
      let ret =
        try Array.get (to_array l ar) v_idx with 
          Invalid_argument _ -> Error.index_err l v_idx
      in (env'', ret)

    | ArrayPut (l, e1, e2, e3) ->
      let (env', ar) = eval_expr env e1 in
      let (env'', idx) = eval_expr env' e2 in
      let (env''', v) = eval_expr env'' e3 
      and v_idx = to_int l idx in
      begin
        (try Array.set (to_array l ar) v_idx v with 
          Invalid_argument _ -> Error.index_err l v_idx);
        (env''', Value.Unit)
      end

    | ArraySize (l, e) ->
      let (env', ar) = eval_expr env e in
        (env', Value.Int (Array.length (to_array l ar)))


(* Evaluate an imp_func given its actual parameters, the source location of the 
 * call, and the environment of the caller. *)
and eval_function f loc actuals env = 
  match f with
    | Value.PrimFunction func -> func loc actuals env
    | Value.UserFunction (formals, locals, body) ->
      let formal_bindings = 
        try 
          List.combine formals actuals
        with Invalid_argument _ ->
          Error.call_err loc ~expected:(List.length formals)
            ~found:(List.length actuals)
      in

      (* Add formal and local bindings to the calling environment. *)
      let env' = Env.bind_locals env (formal_bindings @ locals) in
      (* Evaluate the body of the function. *)
        eval_expr env' body

(* Pretty-print a location to stderr. *)
let print_loc l = Printf.eprintf "    at %s\n%!" (Loc.string_of_loc_short l)

(* Run a unit test. Return whether the test passed. 
 * Prints a message otherwise. *)
let eval_test {type_env; val_env} = function
  | CheckExpectTest (l, to_check, result) ->
    let _ = Typecheck.typecheck_expr type_env to_check in
    let _ = Typecheck.typecheck_expr type_env result in
    let _, actual   = eval_expr val_env to_check in
    let _, expected = eval_expr val_env result in
      if actual <> expected then 
        begin
          Printf.eprintf "check-expect failed: got %s but expected %s\n%!"
            (Value.string_of_value actual) (Value.string_of_value expected);
          print_loc l;
          false
        end 
      else
        true

  | CheckErrorTest (l, to_check) ->
    let _ = Typecheck.typecheck_expr type_env to_check in
      begin 
        try
          begin
            ignore (eval_expr val_env to_check);
            Printf.eprintf "check-error failed: evaluated with no error\n%!";
            print_loc l;
            false
          end
        with Error.Imp_err _ -> 
          true
      end

  | CheckTypeTest (l, to_check, ty) ->
    let expr_ty = Typecheck.typecheck_expr type_env to_check in
      if not (Typecheck.ty_eq expr_ty ty) then 
        begin
          let actual   = Typecheck.string_of_type expr_ty in
          let expected = Typecheck.string_of_type ty in
            begin
              Printf.eprintf 
                "check-type failed: got %s but expected %s\n%!" 
                actual expected;
              print_loc l;
              false
            end
        end 
      else
        true

  | CheckTypeErrorTest (l, to_check) -> 
    begin
      try
        begin
          ignore (Typecheck.typecheck_expr type_env to_check);
          Printf.eprintf 
            "check-type-error failed: didn't get expected type error\n%!";
          print_loc l;
          false
        end
      with
        | Error.Imp_err (_, Error.TypeError _) -> true
        | Error.Imp_err err ->
          begin
            Printf.eprintf "check-type-error failed: got non-type error:\n";
            Error.print_err err;
            false
          end
    end

  | CheckFunTypeTest (l, name, expected_ty) ->
    let actual_ty = Env.lookup_fun type_env l name in        
      if not (Typecheck.fun_ty_eq actual_ty expected_ty) then 
        begin
          let actual   = Typecheck.string_of_function_type actual_ty in
          let expected = Typecheck.string_of_function_type expected_ty in
            begin
              Printf.eprintf 
                "check-function-type failed: got %s but expected %s\n%!"
                actual expected;
              print_loc l;
              false
            end
        end 
      else
        true

(* The evaluator for top-level forms. *)
let rec eval_def ({type_env; val_env} as env) def =
  match def with
    | Val (l, name, e) ->
      let type_env, ty = Typecheck.typecheck_val type_env l name e in
      let val_env, v = eval_expr val_env e in
      let val_env = Env.bind_global val_env name v in
        ({type_env; val_env}, Variable (name, v, ty))

    | Exp (_, e) ->
      let ty = Typecheck.typecheck_expr type_env e in
      let val_env, v = eval_expr val_env e in
        ({type_env; val_env}, Value (v, ty))

    | Define (l, ({name; formals; locals; body; _} as decl)) ->
      let type_env, funty = Typecheck.typecheck_define type_env l decl in
      let locals' = 
        List.map (fun (name, ty) -> (name, default_value_for_type ty)) locals
      in
      let func = Value.UserFunction (List.map fst formals, locals', body) in
      let val_env = Env.bind_fun val_env name func in
        ({type_env; val_env}, Function (name, funty))

    | Use (l, filename) ->
      begin
        try
          load_file filename (Some l) env
        with Parser.Parse_incomplete ->
          Error.syntax_err l 
            (Printf.sprintf 
               "incomplete form at end of file: %s" filename)
      end

    | CheckExpect    (l, _, _)
    | CheckError     (l, _)
    | CheckType      (l, _, _)
    | CheckTypeError (l, _)
    | CheckFunType   (l, _, _) -> Error.unit_test_err l

(* Just like `eval_def`, but additionally keeps track of a list of unit tests
 * and can therefore handle unit test forms. *)
and eval_def_in_use env tests def =
  match def with
    | CheckExpect (l, to_check, result) ->
      let test = CheckExpectTest (l, to_check, result) in
        (env, test :: tests)

    | CheckError (l, to_check) ->
      let test = CheckErrorTest (l, to_check) in
        (env, test :: tests)

    | CheckType (l, to_check, ty) ->
      let test = CheckTypeTest (l, to_check, ty) in
        (env, test :: tests)

    | CheckTypeError (l, to_check) ->
      let test = CheckTypeErrorTest (l, to_check) in
        (env, test :: tests)

    | CheckFunType (l, name, funty) ->
      let test = CheckFunTypeTest (l, name, funty) in
        (env, test :: tests)

    | _ -> let (env', _) = eval_def env def in
      (env', tests)

(* Parse and run the contents of a lexbuf in the current environment.
 *
 * Returns (new_env, num_tests_passed, num_total_tests).
 *)
and use_lexbuf lexbuf filename env =
  (* Parse the file to S-expressions. *)
  let sexprs = Parser.parse_many filename lexbuf in

  (* Parse the S-expressions to top-level AST forms. *)
  let defs = List.map parse_def sexprs in

  (* Helper which folds over the list of definitions in the file,
   * keeping track of modifications to both the list of unit tests and
   * the environment. *)
  let folder (env, unit_tests) def = 
    let (env', unit_tests) = eval_def_in_use env unit_tests def in
      (env', unit_tests)
  in

  (* Finally, actually evaluate the contents of the file. *)
  let (env', unit_tests) = List.fold_left folder (env, []) defs in

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
      (env', Use filename)
    end

