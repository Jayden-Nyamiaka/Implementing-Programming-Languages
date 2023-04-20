exception Parse_error of Loc.loc * string
exception Parse_incomplete

(** Parse a single S-expression from a lexbuf.
    The first argument is a label (usually a filename).
    Return None on EOF. *)
val parse : string -> Lexing.lexbuf -> Sexpr.expr option

(** Parse multiple S-expressions, separated by whitespace, from a lexbuf.
    The first argument is a label (usually a filename) . *)
val parse_many : string -> Lexing.lexbuf -> Sexpr.expr list

(** Parse multiple S-expressions, separated by whitespace, from a string. 
    The first string argument is a label.
    The second string argument contains the literal S-expressions. *)
val parse_many_from_string : string -> string -> Sexpr.expr list

(** Parse S-expressions from a file. 
    The first argument is the filename. *)
val parse_file : string -> Sexpr.expr list

