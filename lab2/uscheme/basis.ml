open Env

(* Basics. *)

let unary_fun f = PrimFuncVal 
  (fun loc vals -> 
     match vals with
       | [a] -> f loc a
       | other -> Error.call_err loc ~expected:1 ~found:(List.length other))

let binary_fun f = PrimFuncVal 
  (fun loc vals -> 
     match vals with
       | [a; b] -> f loc a b
       | other -> Error.call_err loc ~expected:2 ~found:(List.length other))

(* Type queries. *)

let query f = unary_fun (fun _ a -> BoolVal (f a))

let prim_is_boolean = query (function
  | BoolVal _ -> true
  | _ -> false)

let prim_is_num = query (function
  | IntVal _ -> true
  | _ -> false)

let prim_is_nil = query (function
  | NilVal -> true
  | _ -> false)

let prim_is_symbol = query (function
  | SymVal _ -> true
  | _ -> false)

let prim_is_pair = query (function
  | PairVal _ -> true
  | _ -> false)

let prim_is_func = query (function
  | PrimFuncVal _ | UserFuncVal _ -> true
  | _ -> false)

let is_atom = function
  | PairVal _ | PrimFuncVal _ | UserFuncVal _ -> false
  | _ -> true

let prim_is_atom_scheme = query is_atom

(* Equality of atoms. *)
let prim_eq = binary_fun 
  (fun loc a b -> BoolVal 
     (match (a, b) with
        | (NilVal,    NilVal)    -> true
        | (BoolVal a, BoolVal b) -> a = b
        | (IntVal a,  IntVal b)  -> a = b
        | _ when is_atom a && is_atom b -> false
        | _ -> Error.type_err loc ~expected:"two atoms"
                 ~found:(string_of_value a ^ " and " ^ string_of_value b)))

(* Integer operations. *)

let binary_int_fun f = binary_fun 
  (fun loc a b -> 
     match (a, b) with
       | (IntVal a, IntVal b) -> f a b
       | _ -> Error.type_err loc ~expected:"two numbers"
                ~found:(string_of_value a ^ " and " ^ string_of_value b))

let binary_int_int_fun f = binary_int_fun (fun x y -> IntVal (f x y))

let int_comparison f = binary_int_fun (fun x y -> BoolVal (f x y))

(* Division is special because it can raise a divide-by-zero exception. *)
let prim_div = 
  PrimFuncVal
    (fun loc vals ->
       match vals with
         | [IntVal v1; IntVal v2] -> 
           begin
             if v2 = 0 then
               Error.runtime_err loc "division by zero"
             else
               IntVal (v1 / v2)
           end
         | _ -> Error.call_err loc ~expected:2 ~found:(List.length vals))
    
(* List functions. *)

let prim_cons = binary_fun (fun _ a b -> PairVal (a, b))

let prim_car = unary_fun 
  (fun loc v -> 
     match v with
       | PairVal (a, _) -> a
       | _ -> Error.type_err loc ~expected:"pair" ~found:(string_of_value v))

let prim_cdr = unary_fun 
  (fun loc v -> 
     match v with
       | PairVal (_, b) -> b
       | _ -> Error.type_err loc ~expected:"pair" ~found:(string_of_value v))

(* Printing functions. *)

let prim_print = unary_fun
  (fun _ v -> 
    begin
      Printf.printf "%s%!" (string_of_value v);
      UnitVal
    end)

let prim_printc = unary_fun
  (fun loc v ->
     match v with
       | IntVal i -> 
         begin 
           Printf.printf "%c%!" (Char.chr i);
           UnitVal
         end
       | _ -> Error.type_err loc ~expected:"int" ~found:(string_of_value v))

let prim_println = unary_fun
  (fun _ v -> 
    begin
      Printf.printf "%s\n%!" (string_of_value v);
      UnitVal
    end)

(* Error handling. *)

let prim_error = unary_fun
  (fun loc v -> 
    match v with
      | SymVal s -> Error.runtime_err loc s;
      | _ -> Error.type_err loc ~expected:"symbol" ~found:(string_of_value v))


(* ---------------------------------------------------------------------- *)

let basis = make_env
  [("#u",         UnitVal);
   ("#t",         BoolVal true);
   ("#f",         BoolVal false);
   ("_",          UnitVal);       (* Initial binding for "_". *)
   ("nil",        NilVal);
   ("boolean?",   prim_is_boolean);
   ("number?",    prim_is_num);
   ("null?",      prim_is_nil);
   ("pair?",      prim_is_pair);
   ("symbol?",    prim_is_symbol);
   ("procedure?", prim_is_func);
   ("atom?",      prim_is_atom_scheme);
   ("+",          binary_int_int_fun ( + ));
   ("-",          binary_int_int_fun ( - ));
   ("*",          binary_int_int_fun ( * ));
   ("/",          prim_div);
   (">",          int_comparison ( >  ));
   ("<",          int_comparison ( <  ));
   (">=",         int_comparison ( >= ));
   ("<=",         int_comparison ( <= ));
   ("=",          prim_eq);
   ("cons",       prim_cons);
   ("car",        prim_car);
   ("cdr",        prim_cdr);
   ("print",      prim_print);
   ("printc",     prim_printc);
   ("println",    prim_println);
   ("error",      prim_error)
  ] 

let scheme_basis = "
; Boolean functions.
(define not (x) (if x #f #t))

; Equality.
(define != (a b) (not (= a b)))

; TODO: Uncomment this when \"and\" and \"or\" are implemented:
;(define equal? (a b)
;  (if (and (pair? a) (pair? b))
;      (and (equal? (car a) (car b))
;           (equal? (cdr a) (cdr b)))
;      (if (or (pair? a) (pair? b))
;          #f
;          (= a b))))

; List functions.

(define caar  (x) (car (car  x)))
(define cadr  (x) (car (cdr  x)))
(define cdar  (x) (cdr (car  x)))
(define cddr  (x) (cdr (cdr  x)))

(define caaar (x) (car (caar x)))
(define caadr (x) (car (cadr x)))
(define cadar (x) (car (cdar x)))
(define caddr (x) (car (cddr x)))
(define cdaar (x) (cdr (caar x)))
(define cdadr (x) (cdr (cadr x)))
(define cddar (x) (cdr (cdar x)))
(define cdddr (x) (cdr (cddr x)))

(define length (l)
  (letrec ((iter
              (lambda (l accum)
                (if (null? l)
                    accum
                    (iter (cdr l) (+ 1 accum))))))
            (iter l 0)))
(define reverse (a)
  (letrec ((iter
              (lambda (l accum)
                (if (null? l)
                    accum
                    (iter (cdr l) (cons (car l) accum))))))
    (iter a nil)))
(define revapp (a b)
  (if (null? a)
      b
      (revapp (cdr a) (cons (car a) b))))
(define append (a b)
  (revapp (reverse a) b))

; Association lists.
(define bind (k v a)
  (cons (cons k v) a))
(define find (k a)
  (if (null? a)
      #f
      (if (equal? k (caar a))
          (cdar a)
          (find k (cdr a)))))
" 

