(* Intermediate representation (IR). *)

open Sexprlib

open Loc

type id = Ast.id

type lit = Ast.lit

type expr =
  | Literal of loc * lit
  | Quote   of loc * Sexpr.expr
  | Var     of loc * id
  | Set     of loc * id * expr
  | If      of loc * expr * expr * expr
  | While   of loc * expr * expr
  | Begin   of loc * expr list
  | Let     of loc * (id * expr) list * expr
  | LetRec  of loc * (id * expr) list * expr
  | Lambda  of loc * id list * id option * expr
  | Call    of loc * expr * expr list

type def =
  | Val         of loc * id * expr
  | ValRec      of loc * (id * expr) list
  | Use         of loc * string
  | CheckExpect of loc * expr * expr
  | CheckError  of loc * expr

val loc_of_expr : expr -> loc
val loc_of_def  : def -> loc

val ir_of_ast_expr : Ast.expr -> expr
val ir_of_ast_def  : Ast.def  -> def
