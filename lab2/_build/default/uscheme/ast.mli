(* Abstract syntax tree and parsing. *)

open Sexprlib
open Loc

(* Type of identifiers. *)
type id = string

(* Scheme literals. *)
type lit = 
  | Unit 
  | Boolean of bool 
  | Integer of int

(* Scheme expressions. *)
type expr =
  | Literal of loc * lit
  | Quote   of loc * Sexpr.expr
  | Var     of loc * id
  | Set     of loc * id * expr
  | If      of loc * expr * expr * expr
  | Cond    of loc * (expr * expr list) list
  | And     of loc * expr list
  | Or      of loc * expr list
  | While   of loc * expr * expr list
  | Begin   of loc * expr list
  | Let     of loc * (id * expr) list * expr list
  | LetStar of loc * (id * expr) list * expr list
  | LetRec  of loc * (id * expr) list * expr list
  | Lambda  of loc * id list * expr list
  | LambdaX of loc * id list * id * expr list
  | Call    of loc * expr * expr list

(* Scheme top-level forms. *)
type def =
  | Val         of loc * id * expr
  | ValRec      of loc * (id * expr) list
  | Define      of loc * id * id list * expr list
  | DefineX     of loc * id * id list * id * expr list
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
