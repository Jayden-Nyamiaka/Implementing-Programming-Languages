open Ast

type env = 
  {
    type_env : Typecheck.env;
    val_env  : Value.env;
  }

(* Runtime type errors are bugs in the type checker. *)
let ty_err l expected = Error.bug_in_type_checker_err l expected

(* Many of the primitive functions are pure, so we'll make it easy to define
 * such functions. *)
let pure_unary_int f ret =
  let fn l args env =
    match args with
      | [Value.Int arg] -> (env, f arg)
      | _ -> ty_err l "int"
  in
  let signature = {params = [IntType]; ret} in
    (Value.PrimFunction fn, signature)

let pure_binary_int f ret =
  let fn l args env =
    match args with
      | [Value.Int arg1; Value.Int arg2] -> (env, f arg1 arg2)
      | _ -> ty_err l "two ints"
  in
  let signature = {params = [IntType; IntType]; ret} in
    (Value.PrimFunction fn, signature)

(* A pure binary integer-valued function with two integer arguments. *)
let pure_binary_int_int f =
  pure_binary_int (fun x y -> Value.Int (f x y)) IntType

(* A pure binary boolean-valued function with two integer arguments. *)
let comparison f = 
  pure_binary_int (fun x y -> Value.Bool (f x y)) BoolType

(* Division is special because it can raise a divide-by-zero exception. *)
let prim_div = 
  let fval l args env =
    match args with
      | [Value.Int arg1; Value.Int arg2] -> 
        begin
          try 
            (env, Value.Int (arg1 / arg2))
          with Division_by_zero ->
            Error.runtime_err l "division by zero"
        end
      | _ -> ty_err l "two ints"
  in
  let signature = { params = [IntType; IntType]; ret = IntType } in
    (Value.PrimFunction fval, signature)

let printc i =
  begin
    Printf.printf "%c" (Char.chr i);
    Value.Unit
  end

(* An association list of primitive functions (the initial basis). *)
let prims = [ 
  ("+", pure_binary_int_int (+));
  ("-", pure_binary_int_int (-));
  ("*", pure_binary_int_int ( * ));
  ("/", prim_div);
  ("<", comparison (<));
  (">", comparison (>));
  ("printc",  pure_unary_int printc UnitType);
]

let prim_impls = 
  List.map (fun (name, (impl, _)) -> (name, impl)) prims
let prim_sigs = 
  List.map (fun (name, (_, signature)) -> (name, signature)) prims

let prim_vals = [("#u", Value.Unit); ("#t", Value.Bool true); ("#f", Value.Bool false)]
let prim_tys  = [("#u", UnitType); ("#t", BoolType); ("#f", BoolType)]

(* Converting association lists to StringMaps. *)
let assoc_list_to_env funs vals =
  let funs_bound =
    List.fold_left (fun e (n, f) -> Env.bind_fun e n f) Env.empty funs
  in
    List.fold_left (fun e (n, v) -> Env.bind_global e n v) funs_bound vals

(* The initial environment (empty, except for the initial basis). *)
let basis = 
  {
    type_env = assoc_list_to_env prim_sigs prim_tys;
    val_env = assoc_list_to_env prim_impls prim_vals
  }

(* User-level functions that are part of the basis. *)
let user_basis = "
(define bool and ([b : bool] [c : bool]) (if b c b))
(define bool or  ([b : bool] [c : bool]) (if b b c))
(define bool not ([b : bool]) (if b #f #t))
(define bool <=  ([x : int] [y : int]) (not (> x y)))
(define bool >=  ([x : int] [y : int]) (not (< x y)))
(define int  mod ([m : int] [n : int]) (- m (* n (/ m n))))
"

