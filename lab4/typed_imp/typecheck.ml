open Ast

(*
 * Some miscellaneous utilities and shorthands.
 *)

let all_true = List.fold_left (&&) true

let uncurry f (x, y) = f x y

let combine_with f list1 list2 = 
  List.map (uncurry f) (List.combine list1 list2)

type imp_type      = Ast.imp_type
type function_type = Ast.function_type
type env = (imp_type, function_type) Env.t

let rec string_of_type = function
  | IntType     -> "int"
  | UnitType    -> "unit"
  | BoolType    -> "bool"

let string_of_function_type {params; ret} = 
  let rec string_of_formals = function
    | []  -> ""
    | [x] -> string_of_type x
    | x :: xs -> string_of_type x ^ " " ^ string_of_formals xs in
  Printf.sprintf "%s -> %s" (string_of_formals params) (string_of_type ret)

(* Type equality. *)
let rec ty_eq t1 t2 = 
  match (t1, t2) with
    | (IntType,  IntType)  -> true
    | (UnitType, UnitType) -> true
    | (BoolType, BoolType) -> true
    | _ -> false

(* Function-type equality. *)
let fun_ty_eq ty1 ty2 =
  if List.length ty1.params != List.length ty2.params then
    false
  else 
    if not (all_true (combine_with ty_eq ty1.params ty2.params)) then
      false
    else 
      ty_eq ty1.ret ty2.ret

let err l e f = 
  Error.type_err l 
    ~expected: (string_of_type e) 
    ~found:    (string_of_type f)

let ferr l e f = 
  Error.type_err l 
    ~expected: (string_of_function_type e) 
    ~found:    (string_of_function_type f)

let assert_ty_eq l t1 t2 =
  if not (ty_eq t1 t2) then
    Error.type_err l
      ~expected: (string_of_type t1) 
      ~found:    (string_of_type t2)
  else
    ()

let rec typecheck_expr env exp =
  let get_var_ty = Env.lookup_var env in
  let typecheck = typecheck_expr env in
  match exp with
    | Literal (_, UnitLit)   -> UnitType
    | Literal (_, BoolLit _) -> BoolType
    | Literal (_, IntLit _)  -> IntType

    | Var (l, s) -> get_var_ty l s

    | Set (l, s, e) ->
      let expr_ty = typecheck e in
      (* Find the type of `s`. *)
      let var_ty = get_var_ty l s in
        begin
          assert_ty_eq l var_ty expr_ty;
          UnitType
        end

    | If (l, test, t, e) ->
      let testl = loc_of_exp test in
      let test_ty = typecheck test in
        begin
          assert_ty_eq testl BoolType test_ty;
          let t_ty = typecheck t in
          let e_ty = typecheck e in
            if not (ty_eq t_ty e_ty) then 
                err l t_ty e_ty
            else
                t_ty
        end

    | While (_, test, body) ->
      let testl = loc_of_exp test in
      let test_ty = typecheck test in
        begin
          assert_ty_eq testl BoolType test_ty;
          ignore (typecheck body);
          UnitType
        end

    | Begin (_, []) -> UnitType
    (* The type of a `begin` sequence is the type of its last element. *)

    | Begin (_, es) -> List.hd (List.rev_map typecheck es)

    | Call (l, fn, args) ->
      (* Find the type of the function. *)
      let {params; ret} = Env.lookup_fun env l fn in

      (* Then, find the types of the actual parameters. *)
      let actuals = List.map typecheck args in

      (* Check that the numbers of actual and formal parameters match. *)
      let n_actuals = List.length actuals in
      let n_formals = List.length params in
      if n_actuals <> n_formals then
        Error.call_err l ~expected:n_formals ~found:n_actuals
      else
        (* Check that each actual argument type is the same as the
         * corresponding formal argument type. *)
        let rec iter forms acts args =
          match (forms, acts, args) with
            | ([], [], []) -> ret
            | (f::_fs, a::_as, r::_rs) ->
              if not (ty_eq f a) then
                let rl = loc_of_exp r in
                  Error.type_err rl
                    ~expected: (string_of_type f) 
                    ~found:    (string_of_type a)
              else
                iter _fs _as _rs
            | _ -> failwith "this can't happen"
        in
          iter params actuals args

    | Print   (_, e) -> let _ = typecheck e in UnitType
    | Println (_, e) -> let _ = typecheck e in UnitType

    | Eq  (l, e1, e2)
    | Neq (l, e1, e2) ->
      let ty1 = typecheck e1 in 
      let ty2 = typecheck e2 in 
        if ty1 <> ty2 then
          Error.type_err l
            ~expected: "same types"
            ~found:
              (Printf.sprintf "type 1: %s; type 2: %s"
                 (string_of_type ty1)
                 (string_of_type ty2))
        else 
          BoolType


let typecheck_val env l name e = 
  let ty = typecheck_expr env e in
    (* Don't allow the value's type to be redefined. *)
    try
      let old_ty = Env.lookup_var env l name in
        if not (ty_eq ty old_ty) then
          err (loc_of_exp e) old_ty ty
        else
          (env, ty)
    with
      Error.Imp_err (_, NameError _) ->
        (Env.bind_global env name ty, ty)

let typecheck_define env l {ret; name; formals; body} =
  let params = List.map snd formals in
  let fun_ty = {params; ret} in
  let typecheck_body env =
    (* Create a new environment with the arguments bound for type-checking 
     * the body. *)
    let env' = Env.bind_locals env formals in
    let body_ty = typecheck_expr env' body in
      if not (ty_eq body_ty ret) then 
        err l ret body_ty
      else
        ()
  in
  let env' =
    begin
      try
        (* Don't allow the function's type to be redefined. *)
        let old_fun_ty = Env.lookup_fun env l name in
          if not (fun_ty_eq fun_ty old_fun_ty) then
            ferr l old_fun_ty fun_ty
          else
            env
      with
        Error.Imp_err (_, NameError _) ->
          (* Add the function to the environment before type-checking the body
           * to allow recursion. *)
          Env.bind_fun env name fun_ty
    end
  in
    begin
      typecheck_body env';
      (env', fun_ty)
    end

