open Sexprlib

open Env
open Loc
open Ast

(* Part A.5: Question about LetEnvFrame
 * We don't need to push on the LetEnvFrame before executing the Exception
 * Handler because the expection handler is a user function, so it's execution
 * doesn't depend on outer Let or Call environments. User functions can be
 * called within environments that are very different from the environments in
 * which they were defined. And, this is often the case with Exception Handlers
 * as an exception can bypass many function and environment boundaries until
 * reaching a try-catch statement. To reflect this, user functions store 
 * their defining environment, so when a function is applied, it can be 
 * executed in the context of that environment. Because of this, it doesn't
 * matter what the outer environment is, which is why the LetEnvFrame doesn't
 * matter to the Exception Handler.
*)

(* -----------------------------------------------------------------------------
 * Types.
 * ----------------------------------------------------------------------------- *)

(* Unit tests. *)
type unit_test = 
  | CheckExpectTest of loc * expr * expr
  | CheckErrorTest  of loc * expr

(*
 * The uScheme+ interpreter is built around an explicit evaluation context,
 * which consists of a stack of frames.  Each stack frame records what needs to
 * be done with the next value the interpreter produces. 
 *)

(* The information required for a let-frame.  Used for let, let*, and letrec. 
 * Note that let* doesn't need the evaled_bindings list, but making a new
 * type just for that seems excessive. *)
type let_frame = { 
  (* The bindings evaluated so far, as name/value pairs. *)
  evaled_bindings: (id * value) list;

  (* The variable whose evaluation we're awaiting. *)
  current_binding: id;

  (* The bindings that we have not yet started evaluating. *)
  unevaled_bindings: (id * expr) list;

  (* The body of the let expression. *)
  body: expr 
}

(* A stack frame.  The frame on the top of the stack represents what to do with
 * the next value produced, and also allow unwinding the stack for e.g.
 * exception handling. *)

type frame =
  (* Records the variable name whose new value we are evaluating. *)
  | SetFrame of loc * id

  (* Choose the first or second expression to evaluate, depending on whether the
   * current value is true or false. *)
  | IfFrame of loc * expr * expr

  (* Contains the test condition and body of a while loop while we are 
   * evaluating the condition. *)
  | WhileFrame of loc * expr * expr

  (* Contains the test condition and body of a while loop while we are 
   * evaluating the body. *)
  | WhileRunningBodyFrame of loc * expr * expr

  (* Contains the remaining expressions in a `begin` block. *)
  | BeginFrame of loc * expr list

  (* Records the arguments we've evaluated so far, plus the arguments we have
   * yet to evaluate. The arguments evaluated so far are in reverse order (so
   * that function application runs in linear time in the number of arguments),
   * and includes the function itself as the last element in the list. *)
  | ApplyFrame of loc * value list * expr list

  (* Records the caller's environment, which gets restored when the call
   * returns. *)
  | CallEnvFrame of env

  (* We're evaluating the bindings of a let-expression. *)
  | LetFrame of loc * let_frame

  (* We're evaluating the bindings of a let*-expression. *)
  | LetStarFrame of loc * let_frame

  (* We're evaluating the bindings of a let-rec expression. *)
  | LetRecFrame of loc * let_frame

  (* Records the environment before we add let-bindings, which gets restored
   * after we evaluate the body of the let-expression. *)
  | LetEnvFrame of env

  (* We're evaluating the return value. *)
  | ReturnFrame of loc

  (* We're evaluating an exception to throw. *)
  | ThrowFrame of loc

  (* We're evaluating the handler of a try/catch block.  Note that this does not
   * mean that we're in the "try" part!  We have to evaluate the handler first
   * because we need to check that it is a function. When we get the handler,
   * check that it is valid and install it with a `TryCatchFrame`. 
   * Note that the expr here is the _body_ of a `try-catch` expression; it is
   * being held on the stack while the handler is evaluated to a function.
   *)
  | TryCatchEvalHandlerFrame of loc * expr

  (* Contains the handler of a try/catch block.  Exceptions should stop here. *)
  | TryCatchFrame of value

(* Either a value to use to continue evaluating the next frame on the stack, or
 * an expression to evaluate. *)
type item = 
  | Value of value
  | Expr  of expr

(* -----------------------------------------------------------------------------
 * Frame stack debugging functions.
 * ----------------------------------------------------------------------------- *)

let string_of_frame = function
  | ApplyFrame _               -> "ApplyFrame"
  | BeginFrame _               -> "BeginFrame"
  | CallEnvFrame _             -> "CallEnvFrame"
  | IfFrame _                  -> "IfFrame"
  | LetEnvFrame _              -> "LetEnvFrame"
  | LetFrame _                 -> "LetFrame"
  | LetRecFrame _              -> "LetRecFrame"
  | LetStarFrame _             -> "LetStarFrame"
  | ReturnFrame _              -> "ReturnFrame"
  | SetFrame _                 -> "SetFrame"
  | ThrowFrame _               -> "ThrowFrame"
  | TryCatchEvalHandlerFrame _ -> "TryCatchEvalHandlerFrame"
  | TryCatchFrame _            -> "TryCatchFrame"
  | WhileFrame _               -> "WhileFrame"
  | WhileRunningBodyFrame _    -> "WhileRunningBodyFrame"

let print_frame_stack context =
  let contents = Fstack.contents context in
    begin
      Printf.printf "\n";
      Printf.printf "== Frame stack ==\n\n";
      if contents = [] then
        Printf.printf "  <empty>\n"
      else
        List.iter
          (fun f -> Printf.printf "  %s\n" (string_of_frame f))
          contents;
      Printf.printf "\n"
    end

let stack_tracing = ref 0
let frame_stack_max_depth = ref 0

(* Enable or disable frame stack depth tracing.
 * If disabled (i <= 0), return the maximum frame stack depth
 * since it was enabled.
 * If enabled (i > 0), reset the maximum frame stack depth to 0
 * and return 0. *)
let trace_frame_stack i = 
  begin
    assert (i >= 0);
    stack_tracing := i;
    if i > 0 then
      frame_stack_max_depth := 0;
    !frame_stack_max_depth
  end

(* -----------------------------------------------------------------------------
 * Helper functions.
 * ----------------------------------------------------------------------------- *)

let bind_all = List.fold_left (fun e (k, v) -> Env.bind e k v)

let make_let_frame first_name remainder body =
  { 
    evaled_bindings   = [];
    current_binding   = first_name;
    unevaled_bindings = remainder;
    body 
  }

(* Stack functions. *)

let stack_create () = Fstack.create ()

let stack_is_empty s = Fstack.is_empty s

(* This converts the Fstack.Overflow error into a uscheme error. *)
let stack_push l v s = 
  try
    Fstack.push v s
  with
    Fstack.Overflow  -> Error.context_stack_overflow l

let stack_pop   s = Fstack.pop s
let stack_top   s = Fstack.top s
let stack_depth s = Fstack.depth s

(* -----------------------------------------------------------------------------
 * The evaluator.
 * ----------------------------------------------------------------------------- *)

(*
 * Part 1: evaluating expressions.
 *)

(* Push an env frame (LetEnvFrame or CallEnvFrame) unless it would be redundant.
 * Implements tail-call optimization (TCO). *)
let push_env_frame l frame context =
  if !stack_tracing > 0 then
    begin
      Printf.printf "Stack frame depth: %d\n%!" (stack_depth context);
      frame_stack_max_depth := max !frame_stack_max_depth (stack_depth context)
    end;
  if !stack_tracing > 1 then
    print_frame_stack context;
  if stack_is_empty context then stack_push l frame context else
    match (frame, stack_top context) with
      | (_, CallEnvFrame _)
      | (LetEnvFrame _, LetEnvFrame _) -> context
      | (CallEnvFrame _, LetEnvFrame env) -> 
        let (_, context') = stack_pop context in
        stack_push l (CallEnvFrame env) context'
      | _ -> stack_push l frame context

(* Evaluate a function call, where `f` is a function value and `actuals` a list
 * of its actual parameters.
 * Returns an item, an environment, and a context. *)
let eval_call l f actuals env context = 
  match f with
    | PrimFuncVal f -> (Value (f l actuals), env, context)
    | UserFuncVal (formals, body, fenv) -> 
      let context' = push_env_frame l (CallEnvFrame env) context in
        if List.length formals <> List.length actuals then
          Error.call_err l 
            ~expected: (List.length formals)
            ~found: (List.length actuals)
        else
          let fenv' = bind_all fenv (List.combine formals actuals) in
            (Expr body, fenv', context')
    | other -> Error.type_err l 
                 ~expected: "function"
                 ~found: (string_of_value other)

(* Evaluate the value `v` in the context of the given `frame`.
 * Returns an environment, a context, and an item. *)
let eval_val_in_frame v env frame context =
  match frame with
    (* Set the variable to `v`. *)
    | SetFrame (l, var) -> 
      begin
        match v with
          (* Can't write over an unspecified value in a `set`. *)
          | Unspecified s -> 
            let msg = 
              Printf.sprintf "attempt to write to an unspecified value: %s" s
            in
            Error.unspecified_err l msg
          | _ -> 
            begin
              Env.set l env var v;
              (Value UnitVal, env, context)
            end
      end

    (* Check the test condition, and begin evaluating. *)
    | IfFrame (l, then_branch, else_branch) ->
      if Env.truthy l v then
        (Expr then_branch, env, context)
      else
        (Expr else_branch, env, context)

    (* Check the test condition.  If it's true, then move back to the body;
     * otherwise, return unit. *)
    | WhileFrame (l, test, body) ->
      let lt = loc_of_expr test in
        if truthy lt v then 
          let context' = 
            stack_push l (WhileRunningBodyFrame (l, test, body)) context
          in
            (Expr body, env, context')
        else
          (Value UnitVal, env, context)

    (* After evaluating the body, go back to evaluating the test condition. *)
    | WhileRunningBodyFrame (l, test, body) ->
      let context' = stack_push l (WhileFrame (l, test, body)) context in
        (Expr test, env, context')

    (* At the end of a `begin` return the current value
     * (empty `begin`s never push stack frames; see below). *)
    | BeginFrame (_, []) -> (Value v, env, context)

    (* Implementing TCO, a BeginFrame isn't pushed when evaluating the last 
     * expression of a non-empty begin expression. *)
    | BeginFrame (_, [e]) -> (Expr e, env, context)

    (* Throw away the current value, and move on to the rest of the expressions. *)
    | BeginFrame (l, e :: es) ->
      let context' = stack_push l (BeginFrame (l, es)) context in
        (Expr e, env, context')

    | ApplyFrame (l, values_so_far, rest) ->
      begin
        match rest with
          | [] ->
            (* If there are no more arguments to evaluate, call the function. *)
            begin
              let vs      = List.rev (v :: values_so_far) in
              let f       = List.hd vs in
              let actuals = List.tl vs in
                eval_call l f actuals env context
            end

          | e :: remaining ->
            (* If there are more arguments to evaluate, 
             * move the current value onto the evaluated list 
             * and move on to the next one. *)
            let context' = 
              stack_push l (ApplyFrame (l, v :: values_so_far, remaining)) context
            in
              (Expr e, env, context')
      end

    (* Restore the caller environment after a function call. *)
    | CallEnvFrame env' -> (Value v, env', context)

    (* Restore the outer environment after evaluating a let-expression. *)
    | LetEnvFrame env' -> (Value v, env', context)

    (* Once all of the bindings are evaluated, bind everything and start
     * evaluating the body. *)
    | LetFrame (l, { evaled_bindings; 
                     current_binding; 
                     unevaled_bindings; 
                     body }) ->
      begin
        match unevaled_bindings with
          | [] ->   (* no more bindings *)
            let new_bindings = (current_binding, v) :: evaled_bindings in
            let env' = bind_all env new_bindings in
              (Expr body, env', context)

          | (name, expr) :: remainder ->
            (* If there are still more unevaluated bindings, 
             * save the current one in evaled_bindings 
             * and move on to the next one *)
            let new_bindings = (current_binding, v) :: evaled_bindings in
            let new_frame = 
              { 
                evaled_bindings   = new_bindings;
                current_binding   = name;
                unevaled_bindings = remainder;
                body 
              } 
            in
            let context' = stack_push l (LetFrame (l, new_frame)) context in
              (Expr expr, env, context')
      end

    (* The remaining let forms are similar -- 
     * substantial differences are noted below. *)

    | LetStarFrame (l, { evaled_bindings;
                         current_binding;
                         unevaled_bindings;
                         body }) ->
      begin
        match unevaled_bindings with
          | [] ->
            (* For let*, we do the bindings one at a time instead of all at the
             * end.  This also means we never use `evaled_bindings`, which is a
             * bit gross but easier than defining a new `let_star_frame` type 
             * just for let*. *)
            let env' = Env.bind env current_binding v in
              (Expr body, env', context)
          | (name, expr) :: remainder ->
            let env' = Env.bind env current_binding v in
            let new_frame = 
              {
                evaled_bindings;
                current_binding   = name;
                unevaled_bindings = remainder;
                body 
              }
            in
            let context' = stack_push l (LetStarFrame (l, new_frame)) context in
              (Expr expr, env', context')
      end

    | LetRecFrame (l, { evaled_bindings;
                        current_binding;
                        unevaled_bindings;
                        body }) -> 
      begin
        match unevaled_bindings with
          | [] ->
            let new_bindings = (current_binding, v) :: evaled_bindings in
            (* Instead of adding new local bindings, 
             * write over the old (placeholder) ones. *)
            begin
              List.iter (fun (name, v) -> Env.set l env name v) new_bindings;
              (Expr body, env, context)
            end

          | (name, expr) :: remainder ->
            let new_bindings = (current_binding, v) :: evaled_bindings in
            let new_frame = 
              { 
                evaled_bindings   = new_bindings;
                current_binding   = name;
                unevaled_bindings = remainder;
                body 
              } 
            in
            let context' = stack_push l (LetRecFrame (l, new_frame)) context in
              (Expr expr, env, context')
      end

    (* Unwind to the last CallEnvFrame, and then return `v`. *)
    | ReturnFrame l ->
      let rec iter (frm, cntxt) = 
        match frm with 
          | CallEnvFrame env' -> (Value v, env', cntxt)
          | _ -> 
            if stack_is_empty cntxt then Error.return_outside_func l else
            iter (stack_pop cntxt)
      in iter (frame, context)

    (* Check if the handler is valid; if it is, install it with a TryCatchFrame. *)
    | TryCatchEvalHandlerFrame (l, t) -> 
      begin
        match v with
          | PrimFuncVal _
          | UserFuncVal _ ->
            let context'  = push_env_frame l (LetEnvFrame env) context in
            let context'' = stack_push l (TryCatchFrame v) context' in
            (Expr t, env, context'')
          | _ -> Error.type_err l 
                   ~expected: "function" 
                   ~found: (string_of_value v)
      end

    (* If we got here without throwing any exceptions, 
     * just ignore the frame. *)
    | TryCatchFrame _ -> (Value v, env, context)

    (* Unwind to the last TryCatchFrame, and pass `v` to its handler. *)
    | ThrowFrame l ->
      let rec iter (frm, cntxt) = 
        match frm with 
          | TryCatchFrame handler -> eval_call l handler [v] env cntxt
          | _ -> 
            if stack_is_empty cntxt then 
              Error.uncaught_throw l "stack shouldn't be empty" 
            else iter (stack_pop cntxt)
      in iter (frame, context)


(* Reduce an expression to an item.
 * Returns an item, an environment, and a context. *)
let reduce_expr e env context =
  match e with

    (* These forms can be evaluated in one step. *)

    | Literal (_, i) -> (Value (IntVal i), env, context)

    | Quote (_, q) -> (Value (Quote.value_of_sexpr q), env, context)

    | Var (l, s) -> 
      begin 
        match lookup l env s with
          | Unspecified s -> 
            let msg = 
              Printf.sprintf "attempt to read from an unspecified value: %s" s
            in
            Error.unspecified_err l msg
          | other -> (Value other, env, context)
      end

    | Lambda (_, formals, body) -> 
      (Value (UserFuncVal (formals, body, env)), env, context)

    (* These forms push stack frames to tell the evaluator what to do next. *)

    | Set (l, s, e') -> 
      let context' = stack_push l (SetFrame (l, s)) context in
        (Expr e', env, context')

    | If (l, test, e1, e2) ->
      let context' = stack_push l (IfFrame (l, e1, e2)) context in
        (Expr test, env, context')

    | While (l, test, body) ->
      let context' = stack_push l (WhileFrame (l, test, body)) context in
        (Expr test, env, context')

    | Begin (_, []) -> (Value UnitVal, env, context)

    (* Implementing TCO, a BeginFrame isn't pushed when evaluating the last 
     * expression of a non-empty begin expression. *)
    | Begin (_, [e]) -> (Expr e, env, context)

    | Begin (l, e' :: es) -> 
      let context' = stack_push l (BeginFrame (l, es)) context in
        (Expr e', env, context')

    | Let (_, [], body) -> (Expr body, env, context)
    | Let (l, (name, expr) :: remainder, body) ->
      let frame     = make_let_frame name remainder body in
      let context'  = push_env_frame l (LetEnvFrame env) context in
      let context'' = stack_push l (LetFrame (l, frame)) context' in
        (Expr expr, env, context'')

    | LetStar (_, [], body) -> (Expr body, env, context)
    | LetStar (l, (name, expr) :: remainder, body) ->
      let frame     = make_let_frame name remainder body in
      let context'  = push_env_frame l (LetEnvFrame env) context in
      let context'' = stack_push l (LetStarFrame (l, frame)) context' in
        (Expr expr, env, context'')

    | LetRec (_, [], body) -> (Expr body, env, context)
    | LetRec (l, ((name, expr) :: remainder as bindings), body) ->
      let frame = make_let_frame name remainder body in
      let placeholders =
        List.map (fun (name, _) -> (name, Unspecified name)) bindings
      in
      let env' = bind_all env placeholders in
      let context'  = push_env_frame l (LetEnvFrame env) context in
      let context'' = stack_push l (LetRecFrame (l, frame)) context' in
        (Expr expr, env', context'')

    | Call (l, func, args) ->
      let context' = stack_push l (ApplyFrame (l, [], args)) context in
        (Expr func, env, context')

    | Return (l, e') ->
      let context' = stack_push l (ReturnFrame l) context in
        (Expr e', env, context')

    | TryCatch (l, t, c) ->
      let context' = stack_push l (TryCatchEvalHandlerFrame (l, t)) context in
        (Expr c, env, context')

    | Throw (l, e') ->
      let context' = stack_push l (ThrowFrame l) context in
        (Expr e', env, context')

    (* `continue` is special: it doesn't need its own frame because it doesn't
     * take an argument, so it can be evaluated immediately. Just unwind the
     * stack to the last `while` loop. *)
     | Continue l ->
      let rec iter cntxt = 
        if stack_is_empty cntxt then Error.continue_outside_while l else
        let (frame, cntxt') = stack_pop cntxt in
        match frame with 
          | CallEnvFrame _ -> Error.continue_outside_while l
          | LetEnvFrame env' -> (Expr (Continue l), env', cntxt')
          | WhileRunningBodyFrame (lw, test, body) -> 
            let cntxt'' = stack_push l (WhileFrame (lw, test, body)) cntxt'
            in (Expr test, env, cntxt'')
          | _ -> iter cntxt'
      in iter context

    (* `break` is special in the same way continue is. It doesn't need its own
     * frame and can be evaluated immediately. *)
    | Break l ->
      let rec iter cntxt = 
        if stack_is_empty cntxt then Error.break_outside_while l else
        let (frame, cntxt') = stack_pop cntxt in
        match frame with 
          | CallEnvFrame _ -> Error.break_outside_while l
          | LetEnvFrame env' -> (Expr (Break l), env', cntxt')
          | WhileRunningBodyFrame _ -> (Value UnitVal, env, cntxt')
          | _ -> iter cntxt'
      in iter context


(* Evaluate the item `item` to a value.
 * Returns a value, an environment, and a context stack. *)
let rec eval_item item env context =
  match item with
    | Value v ->
      if stack_is_empty context
      then  (* If it's a value, and the stack is empty, we're done. *)
        (v, env, context)
      else
        let (frame, context') = stack_pop context in
        (* Otherwise, feed the value to the next stack frame. *)
        let (new_item, env', context'') = 
          eval_val_in_frame v env frame context'
        in 
          eval_item new_item env' context''
    | Expr e ->
      let (new_item, env', context') =
        reduce_expr e env context
      in 
        eval_item new_item env' context'

(* The expression evaluator. Just calls `eval_item` in a fresh context. 
 * The environment is discarded, since expressions can't permanently
 * change the environment.  The context stack is also discarded, since it 
 * can only be empty once `eval_item` returns.
 *)
let eval_expr env expr =
  let context = stack_create () in
  let (v, _, _) = eval_item (Expr expr) env context in
    v

(* Run a unit test. Return whether the test passed. 
 * Prints a message otherwise. *)
let eval_test env = function
  | CheckExpectTest (loc, to_check, result) ->
    let actual   = eval_expr env to_check in
    let expected = eval_expr env result in
      if actual <> expected then 
        begin
          Printf.eprintf "check-expect failed: got %s but expected %s\n"
            (string_of_value actual) (string_of_value expected);
          Error.print_loc loc;
          false
        end 
      else true

  | CheckErrorTest (loc, to_check) ->
    try
      begin
        ignore (eval_expr env to_check);
        Printf.eprintf "check-error failed: evaluated with no error\n";
        Error.print_loc loc;
        false
      end
    with 
      | Error.UScheme_err _ -> true

(* ----------------------------------------------------------------------------- *)

(*
 * Part 2: evaluating top-level forms.
 *)

(* The evaluator for top-level forms. 
 * Returns the (possibly updated and extended) environment,
 * as well as a value used for printing in the REPL. *)
let rec eval_def env def =
  match def with
    (* Val semantics: If the name is already in the environment, 
     * just overwrite the contents of the existing binding.
     * Otherwise, extend the environment with a new binding. *)
    | Val (l, name, e) ->
      if Env.mem name env then
        let v = eval_expr env e in
          begin
            Env.set l env name v;
            (env, v)
          end
      else
        (* The binding starts off unspecified so that we can define
         * top-level recursive functions. *)
        let env' = Env.bind env name (Unspecified name) in
        let v = eval_expr env' e in
          begin
            Env.set l env' name v;
            (env', v)
          end

    (* Desugar Define into a Val/Lambda combination and then recurse. *)
    | Define (l, name, args, e) -> 
      eval_def env (Val (l, name, Lambda (l, args, e)))

    (* Desugar Exp into a Val using "it" as the name to bind. *)
    | Exp (l, e) -> eval_def env (Val (l, "it", e))

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
    | CheckError (l, _) -> Error.unit_test_err l

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
 * Returns (num_tests_passed, num_total_tests). *)
and use_lexbuf lexbuf filename env =
  (* Parse the file. *)
  let sexprs = Parser.parse_many filename lexbuf in

  (* Helper which folds over the list of definitions in the file,
   * keeping track of modifications to both the list of unit tests and
   * the environment. *)
  let folder (env, unit_tests) expr = 
    eval_def_in_use env unit_tests (parse_def expr) 
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

