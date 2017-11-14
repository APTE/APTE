open Sem_utils

module State : sig
  type t = { hash : int ; left : Configs.t ; right : Configs.t ; constraints : Constraints.t }
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val make : left:Configs.t -> right:Configs.t -> constraints:Constraints.t -> t
end
module Action : sig
  type t =
    | Out of Channel.t * int
    | In of Channel.t * Term.invar list
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val pp_simpl : Format.formatter -> t -> unit (* compact pretty printing *)
end

module ActionSet : sig
  include Set.S with type elt = Action.t
  val pp : Format.formatter -> t -> unit
end

module StateSet : sig
  include Set.S with type elt = State.t
  val pp : Format.formatter -> t -> unit
end

val steps : State.t -> Action.t -> StateSet.t
val fold_successors :
  State.t -> 'a -> (Action.t -> State.t -> 'a -> 'a) -> 'a

val enabled_cover : State.t -> ActionSet.t

val may_be_enabled : State.t -> Action.t -> bool
val may_be_disabled : State.t -> Action.t -> bool

val indep_ee : State.t -> Action.t -> Action.t -> bool
val indep_de : State.t -> Action.t -> Action.t -> bool
