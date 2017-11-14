module type Printable = sig
  type t
  val pp : Format.formatter -> t -> unit
  val pp_simpl : Format.formatter -> t -> unit
end

module type HType = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type HType_act = sig
    include HType
    val pp_simpl : Format.formatter -> t -> unit
  end

module type S = sig

  module State : HType
  module Action : HType_act
		    
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

  (** Return a set of symbolic states whose concretizations contain
    * all enabled actions of all concretizations of the given symbolic
    * state. *)
  val enabled_cover : State.t -> ActionSet.t

  (** [may_be_enabled s a] must return true whenever there
    * exists theta such that [a\theta] is enabled in [s\theta]. *)
  val may_be_enabled : State.t -> Action.t -> bool

  (** [may_be_disabled s a] must return true whenever there
    * exists theta such that [a\theta] is disabled in [s\theta]. *)
  val may_be_disabled : State.t -> Action.t -> bool

  val indep_ee : State.t -> Action.t -> Action.t -> bool
  val indep_de : State.t -> Action.t -> Action.t -> bool

end

module type Simple = sig

  module State : HType
  module Action : Printable

  val fold_successors :
    State.t -> 'a -> (Action.t -> State.t -> 'a -> 'a) -> 'a

end

module Make : functor (T:Simple) -> sig
  module StateSet : Set.S with type elt = T.State.t
  val reachable_states : T.State.t -> StateSet.t
  val nb_traces : T.State.t -> int
  val show_traces : T.State.t -> unit
  type traces = Traces of (T.Action.t*traces) list
  val traces : T.State.t -> traces
  val display_setTraces : traces -> unit
end
