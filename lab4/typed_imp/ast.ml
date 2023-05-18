open Sexprlib

open Loc
open Sexpr

module StringSet = Set.Make(String)

let keywords = [
  "locals";
  "val";
  "define";
  "use";
  "check-expect";
  "check-error";
  "check-type";
  "check-type-error";
  "check-function-type";
  "set";
  "if";
  "while";
  "begin";
  "print";
  "println";
  "=";
  "!=";
  "make-array";
  "array-at";
  "array-put";
  "array-size";
]

let reserved_ids = [
  "#t"; 
  "#f"; 
  "#u";
  "int";
  "bool";
  "unit";
  "array";
]

type id = string

type imp_type = 
  | UnitType
  | BoolType
  | IntType
  | ArrayType of imp_type

type lit =
  | UnitLit
  | BoolLit of bool
  | IntLit  of int

type exp = 
  | Literal   of loc * lit
  | Var       of loc * id
  | Set       of loc * id * exp
  | If        of loc * exp * exp * exp
  | While     of loc * exp * exp
  | Begin     of loc * exp list
  | Call      of loc * id * exp list
  | Print     of loc * exp
  | Println   of loc * exp
  | Eq        of loc * exp * exp
  | Neq       of loc * exp * exp
  | ArrayMake of loc * exp * exp
  | ArrayAt   of loc * exp * exp
  | ArrayPut  of loc * exp * exp * exp 
  | ArraySize of loc * exp

type function_decl = 
  { 
    ret     : imp_type;
    name    : id;
    formals : (id * imp_type) list;
    locals  : (id * imp_type) list;
    body    : exp
  }

type function_type = 
  { 
    params : imp_type list; 
    ret    : imp_type 
  }

type def = 
  | Val            of loc * id * exp
  | Exp            of loc * exp
  | Define         of loc * function_decl
  | Use            of loc * string
  | CheckExpect    of loc * exp * exp
  | CheckError     of loc * exp
  | CheckType      of loc * exp * imp_type
  | CheckTypeError of loc * exp
  | CheckFunType   of loc * id * function_type

let loc_of_exp = function
  | Literal   (l, _)
  | Var       (l, _)
  | Set       (l, _, _)
  | If        (l, _, _, _)
  | While     (l, _, _)
  | Begin     (l, _)
  | Call      (l, _, _)
  | Print     (l, _)
  | Println   (l, _)
  | Eq        (l, _, _)
  | Neq       (l, _, _)
  | ArrayMake (l, _, _)
  | ArrayAt   (l, _, _)
  | ArrayPut  (l, _, _, _)
  | ArraySize (l, _)
      -> l

let loc_of_def = function
  | Val            (l, _, _)
  | Exp            (l, _)
  | Define         (l, _) 
  | Use            (l, _)
  | CheckExpect    (l, _, _)
  | CheckError     (l, _)
  | CheckType      (l, _, _)
  | CheckTypeError (l, _)
  | CheckFunType   (l, _, _)
      -> l

(* This is the only kind of error we'll need. *)
let error = Error.syntax_err

(* ---------------------------------------------------------------------- *)

(*
 * Helper functions for syntax analysis.
 *)

let unique_ids ids =
  StringSet.cardinal (StringSet.of_list ids) = List.length ids

let not_keyword loc name =
  if List.mem name keywords
  then error loc ("keywords can't be variable/function names: " ^ name)
  else name

let not_reserved loc (name : string) : string =
  if List.mem name reserved_ids
  then error loc ("reserved names can't be variable/function names: " ^ name)
  else name

let validate_name loc name =
  let name'  = not_keyword loc name in
  let name'' = not_reserved loc name' in
    name''

(* ---------------------------------------------------------------------- *)

let rec parse_expr = function
  | Int (l, i) -> Literal (l, IntLit i)

  | Id (l, "#u") -> Literal (l, UnitLit)
  | Id (l, "#t") -> Literal (l, BoolLit true)
  | Id (l, "#f") -> Literal (l, BoolLit false)
  | Id (l, name) -> Var (l, not_keyword l name)

  | List (l, [Id (_, "set"); Id (_, name); e]) ->
    let name' = validate_name l name in Set (l, name', parse_expr e)
  | List (l, Id (_, "set") :: _) -> error l "invalid \"set\""

  | List (l, [Id (_, "if"); e1; e2; e3]) ->
    If (l, parse_expr e1, parse_expr e2, parse_expr e3)
  | List (l, Id (_, "if") :: _) -> error l "invalid \"if\""

  | List (l, [Id (_, "while"); e1; e2]) ->
    While (l, parse_expr e1, parse_expr e2)
  | List (l, Id (_, "while") :: _) -> error l "invalid \"while\""

  | List (l, Id (_, "begin") :: es) -> 
    Begin (l, List.map parse_expr es)

  | List (l, [Id (_, "print"); e]) -> Print (l, parse_expr e)
  | List (l, (Id (_, "print") :: _)) -> error l "invalid \"print\""

  | List (l, [Id (_, "println"); e]) -> Println (l, parse_expr e)
  | List (l, (Id (_, "println") :: _)) -> error l "invalid \"println\""

  | List (l, [Id (_, "="); e1; e2]) -> 
    Eq (l, parse_expr e1, parse_expr e2)
  | List (l, (Id (_, "=") :: _)) -> error l "invalid \"=\""

  | List (l, [Id (_, "!="); e1; e2]) -> 
    Neq (l, parse_expr e1, parse_expr e2)
  | List (l, (Id (_, "!=") :: _)) -> error l "invalid \"!=\""

  (* Array forms (built-in polymorphic functions) *)
  | List (l, [Id (_, "make-array"); e1; e2]) -> 
    ArrayMake (l, parse_expr e1, parse_expr e2)
  | List (l, (Id (_, "make-array") :: _)) -> error l "invalid \"make-array\""

  | List (l, [Id (_, "array-at"); e1; e2]) -> 
    ArrayAt (l, parse_expr e1, parse_expr e2)
  | List (l, (Id (_, "array-at") :: _)) -> error l "invalid \"array-at\""

  | List (l, [Id (_, "array-put"); e1; e2; e3]) -> 
    ArrayPut (l, parse_expr e1, parse_expr e2, parse_expr e3)
  | List (l, (Id (_, "array-put") :: _)) -> error l "invalid \"array-put\""

  | List (l, [Id (_, "array-size"); e]) -> ArraySize (l, parse_expr e)
  | List (l, (Id (_, "array-size") :: _)) -> error l "invalid \"array-size\""

  (* If the form is identifiable by a keyword, assume its a function call *)
  | List (l, Id (_, fname) :: es) -> Call (l, fname, List.map parse_expr es)

  | List (l, _) -> error l "unrecognized form"

let rec parse_type = function
  | Id (_, "unit") -> UnitType
  | Id (_, "bool") -> BoolType
  | Id (_, "int")  -> IntType
  | List (_, [Id (_, "array"); typ]) -> 
    ArrayType (parse_type typ)

  | Id (l, _)
  | List (l, _)
  | Int (l, _) 
      -> error l "bad type name"

(* Syntax analysis for definitions. *)
let parse_def v =
  let parse_args al str_arg_type args =  
    let parse_arg =
      let err l = error l ("bad " ^ str_arg_type) in
        function
          | List (l, [Id (_, name); Id (_, ":"); ty]) -> 
            let name' = validate_name l name in
              (name', parse_type ty)
          | List (l, _) -> err l
          | Id   (l, _) -> err l
          | Int  (l, _) -> err l
    in
    let args' = List.map parse_arg args in
    (* Convert the formal parameters to strings. *)
    let arg_names = List.map fst args' in
    if unique_ids arg_names then
      args'
    else
      error al ("non-unique " ^ str_arg_type ^ " names")
  in
  match v with
  | List (l, [Id (_, "val"); Id (_, name); init]) ->
    let name' = validate_name l name in
      Val (l, name', parse_expr init)
  | List (l, Id (_, "val") :: _) -> error l "invalid \"val\""

  | List (l, [Id (_, "define"); ty; Id (_, name); List (fl, formals); body]) ->
    let formals' = parse_args fl "formal parameter" formals in
    let ret      = parse_type ty in
    let body     = parse_expr body in
      Define (l, {ret; name; formals=formals'; locals=[]; body})

  | List (l, [Id (_, "define"); ty; Id (_, name); List (fl, formals); 
      List (ll, Id (_, "locals") :: locals); body]) ->
    let formals' = parse_args fl "formal parameter" formals in
    let locals'  = parse_args ll "local variable" locals in
    let ret      = parse_type ty in
    let body     = parse_expr body in
      Define (l, {ret; name; formals=formals'; locals=locals'; body})

  | List (l, Id (_, "define") :: _) -> error l "invalid \"define\""

  | List (l, [Id (_, "use"); Id (_, filename)]) -> Use (l, filename)
  | List (l, Id (_, "use") :: _) -> error l "invalid \"use\""

  | List (l, [Id (_, "check-expect"); to_check; result]) ->
    CheckExpect (l, parse_expr to_check, parse_expr result)
  | List (l, Id (_, "check-expect") :: _) -> error l "invalid \"check-expect\""

  | List (l, [Id (_, "check-error"); to_check]) ->
    CheckError (l, parse_expr to_check)
  | List (l, Id (_, "check-error") :: _) -> error l "invalid \"check-error\""

  | List (l, [Id (_, "check-type"); to_check; ty]) ->
    CheckType (l, parse_expr to_check, parse_type ty)
  | List (l, Id (_, "check-type") :: _) -> error l "invalid \"check-type\""

  | List (l, [Id (_, "check-type-error"); to_check]) ->
    CheckTypeError (l, parse_expr to_check)
  | List (l, Id (_, "check-type-error") :: _) -> 
    error l "invalid \"check-type-error\""

  | List (l, [Id (_, "check-function-type"); Id (_, name);
              List (_, function_ty)]) ->
    let rec check_funty_helper = function
      | [Id (_, "->"); ty] -> ([], parse_type ty)
      | h :: t ->
        let (params, ret) = check_funty_helper t in
          (parse_type h :: params, ret)
      | _ -> error l "invalid \"check-type\"" 
    in
    let params, ret = check_funty_helper function_ty in
      CheckFunType (l, name, {params; ret})
  | List (l, Id (_, "check-function-type") :: _) ->
      error l "invalid \"check-type\""

  | other -> 
    let exp = parse_expr other in
    let l   = loc_of_exp exp in
      Exp (l, exp)

