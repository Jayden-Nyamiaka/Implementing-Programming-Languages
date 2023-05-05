open Sexprlib

module StringMap = Map.Make(String)

type env = ((value ref) StringMap.t)
and value = 
  | PrimFuncVal of (Loc.loc -> value list -> value)
  | UserFuncVal of Ast.id list * Ast.id option * Ir.expr * env
  | UnitVal
  | BoolVal     of bool
  | IntVal      of int
  | SymVal      of Ast.id
  | NilVal
  | PairVal     of value ref * value ref
  | Unspecified of Ast.id

let rec string_of_value v =
  match v with
    | SymVal _
    | PairVal _ -> "'" ^ _string_of_value v
    | _ -> _string_of_value v

and _string_of_value = function
  | UserFuncVal _  -> "<user function>"
  | PrimFuncVal _  -> "<primitive function>"
  | UnitVal        -> "#u"
  | BoolVal true   -> "#t"
  | BoolVal false  -> "#f"
  | IntVal i       -> string_of_int i
  | SymVal s       -> s 
  | NilVal         -> "nil"
  | PairVal (a, b) -> "(" ^ string_of_pair a b ^ ")"
  | Unspecified s  -> "<unspecified value for " ^ s ^ ">"

and string_of_pair a b =
  let a' =
    match !a with
      | SymVal s -> s
      | _ -> _string_of_value !a
  in
    match !b with
      | NilVal         -> a'
      | PairVal (b, c) -> a' ^ " " ^ string_of_pair b c
      | other          -> a' ^ " . " ^ _string_of_value other

let truthy l = function
  | BoolVal false -> false
  | Unspecified s -> Error.unspecified_err l s
  | _ -> true

let mem name env = StringMap.mem name env

let lookup l env name = 
  match StringMap.find_opt name env with
    | None -> Error.name_err l name
    | Some v -> !v

let set l env name v =
  match StringMap.find_opt name env with
    | None -> Error.name_err l name
    | Some r -> r := v

(* Make a new name/value binding in an environment.
 * If the name already exists, do not overwrite it,
 * but replace the old binding with the new one (functional "update"). *)
let bind env name v = 
  StringMap.add name (ref v) env

let make_env alist = 
  List.fold_left 
    (fun e (n, v) -> StringMap.add n (ref v) e) StringMap.empty alist

