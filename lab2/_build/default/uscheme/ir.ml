open Sexprlib

open Loc

module A = Ast

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
  | Lambda  (l, _, _, _)
  | Call    (l, _, _) -> l

let loc_of_def = function
  | Val         (l, _, _)
  | ValRec      (l, _)
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

    | A.Cond    (l, clauses) ->
      if clauses = [] then Literal (l, Ast.Unit)
      else 
        let rec nest = function
          | [] ->  Literal (l, Ast.Unit)
          | (texp, body) :: rest -> 
            If (l, ir_of_ast_expr texp, wrap_body l body, nest rest)
        in nest clauses 
      
    | A.And     (l, exprs) -> 
      let rec nest = function
        | [] -> Literal (l, Ast.Boolean (true))
        | [e] -> ir_of_ast_expr e
        | e :: rest -> 
          If (l, ir_of_ast_expr e, nest rest, Literal (l, Ast.Boolean (false)))
      in nest exprs

    | A.Or      (l, exprs) ->
      let rec nest = function
        | [] -> Literal (l, Ast.Boolean (false))
        | [e] -> ir_of_ast_expr e
        | e :: rest -> Let (l, [("{or}", ir_of_ast_expr e)], 
              If (l, Var (l, "{or}"), Var (l, "{or}"), nest rest))
      in nest exprs

    | A.While   (l, e1, body) ->
      While (l, ir_of_ast_expr e1, wrap_body l body)

    | A.Begin   (l, es) ->
      Begin (l, List.map ir_of_ast_expr es)

    | A.Let     (l, bindings, body) ->
      let bindings' = List.map ir_of_ast_binding bindings in
        Let (l, bindings', wrap_body l body)

    | A.LetStar (l, bindings, body) -> 
      let rec nest binds =
        match binds with 
        | [] -> wrap_body l body
        | h :: rest -> Let (l, [ir_of_ast_binding h], nest rest)
      in nest bindings 

    | A.LetRec  (l, bindings, body) ->
      let bindings' = List.map ir_of_ast_binding bindings in
        LetRec (l, bindings', wrap_body l body)

    | A.Lambda  (l, names, body) -> 
      Lambda (l, names, None, wrap_body l body)
    | A.LambdaX (l, names, rest, body) -> 
      Lambda (l, names, Some (rest), wrap_body l body)

    | A.Call    (l, e, es) ->
      Call (l, ir_of_ast_expr e, List.map ir_of_ast_expr es)

and ir_of_ast_binding (name, e) = (name, ir_of_ast_expr e)

and wrap_body l body = 
  match body with 
    | [single_exp] -> ir_of_ast_expr single_exp
    | _ -> Begin (l, List.map ir_of_ast_expr body)

let ir_of_ast_def adef =
  match adef with
    | A.Val     (l, s, e) -> Val (l, s, ir_of_ast_expr e)

    | A.ValRec  (l, bindings) -> ValRec (l, List.map ir_of_ast_binding bindings)

    | A.Define  (l, name, args, body) -> 
      Val (l, name, Lambda (l, args, None, wrap_body l body))
    | A.DefineX (l, name, args, rest, body) -> 
      Val (l, name, Lambda (l, args, Some (rest), wrap_body l body))
    
    | A.Exp     (l, e) -> Val (l, "_", ir_of_ast_expr e)

    | A.Use     (l, s) -> Use (l, s)

    | A.CheckExpect (l, e1, e2) ->
      CheckExpect (l, ir_of_ast_expr e1, ir_of_ast_expr e2)

    | A.CheckError  (l, e) -> CheckError (l, ir_of_ast_expr e)

