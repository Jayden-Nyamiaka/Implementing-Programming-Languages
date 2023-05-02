(* Abstract syntax tree and parsing. *)

open Sexprlib
open Loc

(* Type of identifiers. *)
type id = string

(* Scheme expressions. *)
type expr = 
  | Literal of loc * int
  | Quote   of loc * Sexpr.expr
  | Var     of loc * id
  | Set     of loc * id * expr
  | If      of loc * expr * expr * expr
  | While   of loc * expr * expr
  | Begin   of loc * expr list
  | Let     of loc * (id * expr) list * expr
  | LetStar of loc * (id * expr) list * expr
  | LetRec  of loc * (id * expr) list * expr
  | Lambda  of loc * id list * expr
  | Call    of loc * expr * expr list

(* Scheme top-level forms. *)
type def = 
  | Val         of loc * id * expr
  | Define      of loc * id * id list * expr
  | Exp         of loc * expr
  | Use         of loc * string
  | CheckExpect of loc * expr * expr
  | CheckError  of loc * expr

val loc_of_expr : expr -> loc
val loc_of_def  : def -> loc

(*
 * Syntax analysis.
 *)

val parse_expr : Sexpr.expr -> expr
val parse_def  : Sexpr.expr -> def
