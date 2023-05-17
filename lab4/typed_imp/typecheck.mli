(* The type checker. *)

open Sexprlib

(* Note that the syntactic and abstract representations of typed-imp types are
 * essentially identical, so the internal representation we use is just the AST
 * representation. *)

type imp_type = Ast.imp_type
type function_type = Ast.function_type

(* The type environment: *)
type env = (imp_type, function_type) Env.t

(* Checking type equivalence. *)
val ty_eq : imp_type -> imp_type -> bool

(* Checking function-type equivalence. *)
val fun_ty_eq : function_type -> function_type -> bool

(* Converting types to strings. *)
val string_of_type : imp_type -> string
val string_of_function_type : function_type -> string

(* Typecheck an expression and return its type. *)
val typecheck_expr : env -> Ast.exp -> imp_type

(* Typecheck a "define" form, modifying the type environment. *)
val typecheck_define : 
  env -> Loc.loc -> Ast.function_decl -> env * function_type

(* Typecheck a "val" form, modifying the type environment. *)
val typecheck_val : 
  env -> Loc.loc -> Ast.id -> Ast.exp -> env * imp_type

