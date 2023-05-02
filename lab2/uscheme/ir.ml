open Sexprlib

open Loc

module A = Ast

type id = Ast.id

type expr =
  | Literal of loc * int
  | Quote   of loc * Sexpr.expr
  | Var     of loc * id
  | Set     of loc * id * expr
  | If      of loc * expr * expr * expr
  | While   of loc * expr * expr
  | Begin   of loc * expr list
  | Let     of loc * (id * expr) list * expr
  | LetRec  of loc * (id * expr) list * expr
  | Lambda  of loc * id list * expr
  | Call    of loc * expr * expr list

type def =
  | Val         of loc * id * expr
  | Use         of loc * string
  | CheckExpect of loc * expr * expr
  | CheckError  of loc * expr

let loc_of_expr = function
  | Literal (l, _)
  | Quote   (l, _)
  | Var     (l, _)
  | Set     (l, _, _)
  | If      (l, _, _, _)
  | While   (l, _, _)
  | Begin   (l, _)
  | Let     (l, _, _)
  | LetRec  (l, _, _)
  | Lambda  (l, _, _)
  | Call    (l, _, _) -> l

let loc_of_def = function
  | Val         (l, _, _)
  | Use         (l, _)
  | CheckExpect (l, _, _)
  | CheckError  (l, _) -> l

(* ---------------------------------------------------------------------- *)

(* Converting AST forms to IR forms. *)

let rec ir_of_ast_expr aexpr =
  match aexpr with
    | A.Literal (l, lt) -> Literal (l, lt)
    | A.Quote   (l, e) -> Quote (l, e)
    | A.Var     (l, s) -> Var (l, s)
    | A.Set     (l, s, e) -> Set (l, s, ir_of_ast_expr e)
    | A.If      (l, e1, e2, e3) ->
      If (l, ir_of_ast_expr e1, ir_of_ast_expr e2, ir_of_ast_expr e3)

    | A.While   (l, e1, e2) ->
      While (l, ir_of_ast_expr e1, ir_of_ast_expr e2)

    | A.Begin   (l, es) ->
      Begin (l, List.map ir_of_ast_expr es)

    | A.Let     (l, bindings, e) ->
      let bindings' = List.map ir_of_ast_binding bindings in
        Let (l, bindings', ir_of_ast_expr e)

    | A.LetStar (l, bindings, e) -> failwith "TODO"

    | A.LetRec  (l, bindings, e) ->
      let bindings' = List.map ir_of_ast_binding bindings in
        LetRec (l, bindings', ir_of_ast_expr e)

    | A.Lambda  (l, names, e) -> Lambda (l, names, ir_of_ast_expr e)

    | A.Call    (l, e, es) ->
      Call (l, ir_of_ast_expr e, List.map ir_of_ast_expr es)

and ir_of_ast_binding (name, e) = (name, ir_of_ast_expr e)

let ir_of_ast_def adef =
  match adef with
    | A.Val    (l, s, e) -> Val (l, s, ir_of_ast_expr e)
    | A.Define (l, name, args, e) -> failwith "TODO"
    | A.Exp    (l, e) -> failwith "TODO"

    | A.Use    (l, s) -> Use (l, s)

    | A.CheckExpect (l, e1, e2) ->
      CheckExpect (l, ir_of_ast_expr e1, ir_of_ast_expr e2)

    | A.CheckError  (l, e) -> CheckError (l, ir_of_ast_expr e)

