(** Representation of processes, hash-consed. *)

type term = Frame.Term.t
type formula = Formula.t
		 
type ('a,'t,'f) _proc =
  Zero
  | Par of 'a list
  | Plus of 'a list
  | If of 'f * 'a * 'a
  | Input of Channel.t * 't * 'a
  | Output of Channel.t * 't * 'a
  | Bottom of int
		
type proc = private { id : int; contents : (proc,term,formula) _proc; }
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
val if_form : formula -> proc -> proc -> proc
val if_eq : term -> term -> proc -> proc -> proc
val if_neq : term -> term -> proc -> proc -> proc

(** Test over all subprocesses under a specific construct *)

val for_all_plus : (proc -> bool) -> proc -> bool
val exists_par : (proc -> bool) -> proc -> bool

(** Substitution function *)
val subst : proc -> term -> term -> proc

(** Pre-transitions *)
val transitions :
  proc -> ((term -> proc) list, (term * proc) list) Channel.io_map
