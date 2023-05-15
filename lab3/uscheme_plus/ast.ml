open Sexprlib

open Loc
open Sexpr

module StringSet = Set.Make(String)

let error = Error.syntax_err

let keywords = [
  "val";
  "define";
  "use";
  "check-expect";
  "check-error";
  "set";
  "if";
  "while";
  "begin";
  "let";
  "let*";
  "lambda";
  "return";
  "try-catch";
  "throw";
  "continue";
  "break";
]

let reserved_ids = ["#t"; "#f"; "#u"; "nil"]

type id = string

type expr =
  | Literal  of loc * int
  | Quote    of loc * Sexpr.expr
  | Var      of loc * id
  | Set      of loc * id * expr
  | If       of loc * expr * expr * expr
  | While    of loc * expr * expr
  | Begin    of loc * expr list
  | Let      of loc * (id * expr) list * expr
  | LetStar  of loc * (id * expr) list * expr
  | LetRec   of loc * (id * expr) list * expr
  | Lambda   of loc * id list * expr
  | Call     of loc * expr * expr list
  | Return   of loc * expr
  | TryCatch of loc * expr * expr
  | Throw    of loc * expr
  | Continue of loc
  | Break    of loc

type def =
  | Val         of loc * id * expr
  | Define      of loc * id * id list * expr
  | Exp         of loc * expr
  | Use         of loc * string
  | CheckExpect of loc * expr * expr
  | CheckError  of loc * expr

let loc_of_expr = function
  | Literal  (l, _)
  | Quote    (l, _)
  | Var      (l, _)
  | Set      (l, _, _)
  | If       (l, _, _, _)
  | While    (l, _, _)
  | Begin    (l, _)
  | Let      (l, _, _)
  | LetStar  (l, _, _)
  | LetRec   (l, _, _)
  | Lambda   (l, _, _)
  | Call     (l, _, _)
  | Return   (l, _)
  | TryCatch (l, _, _)
  | Throw    (l, _)
  | Continue l
  | Break    l
      -> l

let loc_of_def = function
  | Val         (l, _, _)
  | Define      (l, _, _, _)
  | Exp         (l, _)
  | Use         (l, _)
  | CheckExpect (l, _, _)
  | CheckError  (l, _)
      -> l

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

(* ---------------------------------------------------------------------- *)

(*
 * Main parsing functions.
 *)

let rec parse_expr = function
  | Int (l, i) -> Literal (l, i)

  | Id (l, name) -> Var (l, not_keyword l name)

  | List (l, [Id (_, "quote"); e]) ->
    Quote (l, e)
  | List (l, (Id (_, "quote") :: _)) -> error l "invalid \"quote\""

  | List (l, [Id (_, "set"); Id (_, name); e]) ->
    let name' = validate_name l name in
      Set (l, name', parse_expr e)
  | List (l, Id (_, "set") :: _) -> error l "invalid \"set\""

  | List (l, [Id (_, "if"); e1; e2; e3]) ->
    If (l, parse_expr e1, parse_expr e2, parse_expr e3)
  | List (l, Id (_, "if") :: _) -> error l "invalid \"if\""

  | List (l, [Id (_, "while"); e1; e2]) ->
    While (l, parse_expr e1, parse_expr e2)
  | List (l, Id (_, "while") :: _) -> error l "invalid \"while\""

  | List (l, Id (_, "begin") :: es) -> Begin (l, List.map parse_expr es)

  | List (l, [Id (_, "let"); List (_, bindings); e]) ->
    let ids = List.map get_binding_name bindings in
      if unique_ids ids then
        Let (l, List.map parse_binding bindings, parse_expr e)
      else
        error l "duplicated \"let\" bindings"
  | List (l, Id (_, "let") :: _) -> error l "invalid \"let\""

  | List (l, [Id (_, "let*"); List (_, bindings); e]) ->
    let ids = List.map get_binding_name bindings in
      if unique_ids ids then
        LetStar (l, List.map parse_binding bindings, parse_expr e)
      else
        error l "duplicated \"let*\" bindings"
  | List (l, Id (_, "let*") :: _) -> error l "invalid \"let*\""

  | List (l, [Id (_, "letrec"); List (_, bindings); e]) ->
    let ids = List.map get_binding_name bindings in
      if unique_ids ids then
        LetRec (l, List.map parse_binding bindings, parse_expr e)
      else
        error l "duplicated \"letrec\" bindings"
  | List (l, Id (_, "letrec") :: _) -> error l "invalid \"letrec\""

  | List (l, [Id (_, "lambda"); List (_, formals); e]) ->
    let formals' = List.map get_formal_name formals in
      if unique_ids formals' then
        Lambda (l, formals', parse_expr e)
      else
        error l "duplicated \"lambda\" bindings"
  | List (l, Id (_, "lambda") :: _) -> error l "invalid \"lambda\""

  | List (l, [Id (_, "return"); e]) -> Return (l, parse_expr e)
  | List (l, Id (_, "return") :: _) -> error l "invalid \"return\""

  | List (l, [Id (_, "try-catch"); t; c]) ->
    TryCatch (l, parse_expr t, parse_expr c)
  | List (l, Id (_, "try-catch") :: _) -> error l "invalid \"try-catch\""

  | List (l, [Id (_, "throw"); e]) -> Throw (l, (parse_expr e))
  | List (l, Id (_, "throw") :: _) -> error l "invalid \"throw\""

  | List (l, [Id (_, "continue")]) -> Continue l
  | List (l, Id (_, "continue") :: _) -> error l "invalid \"continue\""

  | List (l, [Id (_, "break")]) -> Break l
  | List (l, Id (_, "break") :: _) -> error l "invalid \"break\""

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
  | List (l, Id (_, "val") :: _) -> error l "invalid \"val\""

  | List (l, [Id (_, "define"); Id (_, name); List (_, args); body]) ->
    let name' = validate_name l name in
    let args' = List.map get_formal_name args in
      if unique_ids args' then
        Define (l, name', args', parse_expr body)
      else
        error l "duplicated \"define\" bindings"
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

