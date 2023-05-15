(* Simple functional stack implementation. *)

(* The stack type. *)
type 'a t

exception Underflow
exception Overflow

val max_size     : int ref
val set_max_size : int -> unit
val create       : unit -> 'a t
val contents     : 'a t -> 'a list
val depth        : 'a t -> int
val is_empty     : 'a t -> bool
val push         : 'a -> 'a t -> 'a t
val pop          : 'a t -> 'a * 'a t
val top          : 'a t -> 'a

