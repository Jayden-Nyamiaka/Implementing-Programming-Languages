(* The AST and syntax analysis. *)

open Sexprlib
open Loc

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
  (* Printing, equality and array functions are polymorphic, so we need to
   * special-case them because the type system doesn't support
   * polymorphic types. *)
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

(* Top-level forms. *)
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

val loc_of_exp : exp -> loc
val loc_of_def : def -> loc
val parse_expr : Sexpr.expr -> exp
val parse_type : Sexpr.expr -> imp_type
val parse_def  : Sexpr.expr -> def

