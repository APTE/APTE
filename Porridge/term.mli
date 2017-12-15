(** Term representation, hash-consed. *)

type 'a _term
type term = private { id : int ; contents : term _term }
type t = term
val equal : term -> term -> bool
val compare : term -> term -> int
val hash : term -> int

val pp : Format.formatter -> term -> unit
val to_string : term -> string

val ok : unit -> term
val senc : term -> term -> term
val sdec : term -> term -> term
val aenc : term -> term -> term
val adec : term -> term -> term
val pk : term -> term
val mac : term -> term -> term
val hash_tm : term -> term
val sign : term -> term -> term
val checksign : term -> term -> term
val vk : term -> term
val hash : term -> term
val tuple : term list -> term

(** Variables for representing symbolic inputs. *)
type invar = Channel.t*int*int
val invar : Channel.channel -> n:int -> phi:int -> term

(** Variables for representing bindings and bound variables
  * in processes. *)
val var : string -> term

val subst : term -> term -> term -> term
