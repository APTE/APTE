(** Channels are represented by integers for technical convenience
  * in the of the semantics. Their number is fixed (hard-coded) and
  * the [channel] function must be used to create a channel from
  * an integer, which is checked for validity. *)
type channel = private int
type t = channel
val nb_chan : int
val of_int : int -> channel
val to_int : channel -> int
val to_char : channel -> char

val c : channel
val d : channel
val e : channel
