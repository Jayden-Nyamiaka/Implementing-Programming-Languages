open Sexprlib

open Loc
open Sexpr

module StringSet = Set.Make(String)

let error = Error.syntax_err

let keywords = ["val"; "define"; "use"; "check-expect"; "check-error";
                "quote"; "set"; "if"; "while"; "begin"; "let"; "let*";
                "letrec"; "lambda"; "cond"; "and"; "or"; "valrec"]

let reserved_ids = ["nil"; "."]

type id = string

type lit = 
  | Unit 
  | Boolean of bool 
  | Integer of int

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

type def =
  | Val         of loc * id * expr
  | ValRec      of loc * (id * expr) list
  | Define      of loc * id * id list * expr list
  | DefineX     of loc * id * id list * id * expr list
  | Exp         of loc * expr
  | Use         of loc * string
  | CheckExpect of loc * expr * expr
  | CheckError  of loc * expr

let loc_of_expr = function
  | Literal (l, _)
  | Quote   (l, _)
  | Var     (l, _)
  | Set     (l, _, _)
  | If      (l, _, _, _)
  | Cond    (l, _)
  | And     (l, _)
  | Or      (l, _)
  | While   (l, _, _)
  | Begin   (l, _)
  | Let     (l, _, _)
  | LetStar (l, _, _)
  | LetRec  (l, _, _)
  | Lambda  (l, _, _)
  | LambdaX (l, _, _, _)
  | Call    (l, _, _) -> l

let loc_of_def = function
  | Val         (l, _, _)
  | ValRec      (l, _)
  | Define      (l, _, _, _)
  | DefineX     (l, _, _, _, _)
  | Exp         (l, _)
  | Use         (l, _)
  | CheckExpect (l, _, _)
  | CheckError  (l, _) -> l

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

let not_dot loc (name : string) : string =
  if name = "."
  then error loc ("\".\" can't be a variable/function name")
  else name

let validate_name loc name =
  let name'   = not_keyword loc name in
  let name''  = not_reserved loc name' in
  let name''' = not_dot loc name'' in
    name'''

let get_formal_name expr =
  match expr with
    | Id (l, name) ->
      let name' = validate_name l name in name'
    | _ -> error (Sexpr.loc_of_expr expr) "invalid_formal_parameter"

let get_binding_name expr =
  match expr with
    | List (_, [Id (l, name); _]) ->
      let name' = validate_name l name in name'
    | _ -> error (Sexpr.loc_of_expr expr) "invalid binding"

(* Gets formals (and optional "rest" parameter) for lambda(X) and define(X).
   Returns a tuple of the list of formals and the optional rest parameter. *)
let rec parse_params l type_name forms count save = 
  match forms with 
    | [] -> 
      if unique_ids save then (List.rev save, None)
      else error l ("duplicated \"" ^ type_name ^ "\" bindings")
    | Id (_, fname) :: t when fname = "." ->
      if count = 0 || (List.length t) <> 1 
        then error l ("\".\" use within \"" 
                          ^ type_name ^ "\" is invalid")
        else
          let rest_param = get_formal_name (List.hd t) in
            if rest_param = "." then
              error l ("\".\" use within \"" 
                                        ^ type_name ^ "\" is invalid")
            else if unique_ids (rest_param :: save) 
              then (List.rev save, Some (rest_param) ) 
              else error l ("duplicated \"" ^ type_name ^ "\" bindings") 
    | formal :: t -> parse_params l type_name t (count + 1) 
                        ((get_formal_name formal) :: save)

(* ---------------------------------------------------------------------- *)

(*
 * Main parsing functions.
 *)

let rec parse_expr = function
  | Int (l, i)    -> Literal (l, Integer (i))
  | Id  (l, "#u") -> Literal (l, Unit)
  | Id  (l, "#t") -> Literal (l, Boolean (true))
  | Id  (l, "#f") -> Literal (l, Boolean (false))

  | Id (l, name) ->
    Var (l, not_keyword l name)

  | List (l, [Id (_, "quote"); e]) -> Quote (l, e)
  | List (l, (Id (_, "quote") :: _)) -> error l "invalid \"quote\""

  | List (l, [Id (_, "set"); Id (_, name); e]) ->
    let name' = validate_name l name in
      Set (l, name', parse_expr e)
  | List (l, Id (_, "set") :: _) -> error l "invalid \"set\""

  | List (l, [Id (_, "if"); e1; e2; e3]) ->
    If (l, parse_expr e1, parse_expr e2, parse_expr e3)
  | List (l, Id (_, "if") :: _) -> error l "invalid \"if\""

  | List (l, Id (_, "cond") :: clauses) ->
    let per_clause = function
      | List (_, texp :: body) -> (parse_expr texp, List.map parse_expr body)
      | _ -> error l "invalid \"cond\""
    in Cond (l, List.map per_clause clauses)

  | List (l, Id (_, "and") :: exprs) -> And (l, List.map parse_expr exprs)
  | List (l, Id (_, "or") :: exprs) -> Or (l, List.map parse_expr exprs)
  
  | List (l, Id (_, "while") :: e1 :: e) ->
    While (l, parse_expr e1, List.map parse_expr e)
  | List (l, Id (_, "while") :: _) -> error l "invalid \"while\""

  | List (l, Id (_, "begin") :: es) -> Begin (l, List.map parse_expr es)

  | List (l, Id (_, "let") :: List (_, bindings) :: (_ :: _ as e)) ->
    let ids = List.map get_binding_name bindings in
      if unique_ids ids then
        Let (l, List.map parse_binding bindings, List.map parse_expr e)
      else
        error l "duplicated \"let\" bindings"
  | List (l, Id (_, "let") :: _) -> error l "invalid \"let\""

  | List (l, Id (_, "let*") :: List (_, bindings) :: (_ :: _ as e)) ->
    let ids = List.map get_binding_name bindings in
      if unique_ids ids then
        LetStar (l, List.map parse_binding bindings, List.map parse_expr e)
      else
        error l "duplicated \"let*\" bindings"
  | List (l, Id (_, "let*") :: _) -> error l "invalid \"let*\""

  | List (l, Id (_, "letrec") :: List (_, bindings) :: (_ :: _ as e)) ->
    let ids = List.map get_binding_name bindings in
      if unique_ids ids then
        LetRec (l, List.map parse_binding bindings, List.map parse_expr e)
      else
        error l "duplicated \"letrec\" bindings"
  | List (l, Id (_, "letrec") :: _) -> error l "invalid \"letrec\""

  | List (l, Id (_, "lambda") :: List (_, formals) :: (_ :: _ as e)) -> 
    let (formals', rest_opt) = parse_params l "lambda" formals 0 [] in
      if rest_opt = None then
        Lambda (l, formals', List.map parse_expr e)
      else 
        LambdaX (l, formals', Option.get rest_opt, List.map parse_expr e)

  | List (l, Id (_, "lambda") :: (Id _ as rest_param) :: (_ :: _ as e)) ->
    let rest_param' = get_formal_name rest_param in
      LambdaX (l, [], rest_param', List.map parse_expr e)

  | List (l, Id (_, "lambda") :: _) -> error l "invalid \"lambda\""

  | List (l, func :: es) -> Call (l, parse_expr func, List.map parse_expr es)

  | List (l, _) -> error l "unrecognized form"

and parse_binding expr =
  match expr with
    | List (_, [Id (_, name); expr]) -> (name, parse_expr expr)
    | _ -> error (Sexpr.loc_of_expr expr) "invalid binding"

let parse_def = function
  | List (l, [Id (_, "val"); Id (_, name); init]) ->
    let name' = validate_name l name in
      Val (l, name', parse_expr init)
  | List (l, (Id (_, "val") :: _)) ->
      error l "invalid \"val\""

  | List (l, Id (_, "valrec") :: (_ :: _ as bindings)) ->
    let ids = List.map get_binding_name bindings in
      if unique_ids ids then
        ValRec (l, List.map parse_binding bindings)
      else
        error l "duplicated \"valrec\" bindings"
  | List (l, Id (_, "valrec") :: _) -> error l "invalid \"valrec\""

  | List (l, Id (_, "define") :: Id (_, name) :: List (_, args) 
                                        :: (_ :: _ as e)) -> 
    let name' = validate_name l name 
    and (args', rest_opt) = parse_params l "define" args 0 [] in
      if rest_opt = None then
        Define (l, name', args', List.map parse_expr e)
      else 
        DefineX (l, name', args', Option.get rest_opt, List.map parse_expr e)

  | List (l, Id (_, "define") :: Id (_, name) :: (Id _ as rest_param) 
                                              :: (_ :: _ as e)) ->
    let name' = validate_name l name 
    and rest_param' = get_formal_name rest_param in
      DefineX (l, name', [], rest_param', List.map parse_expr e)

  | List (l, Id (_, "define") :: _) -> error l "invalid \"define\""

  | List (l, [Id (_, "use"); Id (_, filename)]) -> Use (l, filename)
  | List (l, Id (_, "use") :: _) -> error l "invalid \"use\""

  | List (l, [Id (_, "check-expect"); to_check; result]) ->
    CheckExpect (l, parse_expr to_check, parse_expr result)
  | List (l, Id (_, "check-expect") :: _) -> error l "invalid \"check-expect\""

  | List (l, [Id (_, "check-error"); to_check]) ->
    CheckError (l, parse_expr to_check)
  | List (l, Id (_, "check-error") :: _) -> error l "invalid \"check-error\""

  | other -> let expr = parse_expr other in Exp (loc_of_expr expr, expr)

