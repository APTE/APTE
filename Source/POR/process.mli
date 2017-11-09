(** Representation of processes, hash-consed. *)

type term = Term.term

type ('a,'t) _proc =
    Zero
  | Par of 'a list
  | Plus of 'a list
  | If of 't * 't * 'a * 'a
  | Input of Channel.t * 't * 'a
  | Output of Channel.t * 't * 'a
  | Bottom of int

type proc = private { id : int; contents : (proc,term) _proc; }
type t = proc

val equal : proc -> proc -> bool
val compare : proc -> proc -> int
val hash : proc -> int

val pp : Format.formatter -> proc -> unit

(** Smart constructors providing hash consing *)

val zero : proc
val bottom : int -> proc
val input : Channel.t -> term -> proc -> proc
val output : Channel.t -> term -> proc -> proc
val par : proc list -> proc
val plus : proc list -> proc
val if_eq : term -> term -> proc -> proc -> proc
val if_neq : term -> term -> proc -> proc -> proc

(** Substitution function *)
val subst : proc -> term -> term -> proc
