module type Printable = sig
  type t
  val pp : Format.formatter -> t -> unit
end

module type HType = sig
  include Printable
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module type Printable_simpl = sig
  include Printable
  val pp_simpl : Format.formatter -> t -> unit
end

module type HType_simpl = sig
  include HType
  val pp_simpl : Format.formatter -> t -> unit
end

module type S = sig

  module State : HType
  module Action : HType_simpl
  module SemanticAction : HType_simpl with type t = Action.t

  module ActionSet : sig
    include Set.S with type elt = Action.t
    val pp : Format.formatter -> t -> unit
  end

  module SemanticActionSet : sig
    type t
    type elt = Action.t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val union : t -> t -> t
    val choose : t -> elt*t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val cardinal : t -> int
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val pp : Format.formatter -> t -> unit
  end

  module StateSet : sig
    include Set.S with type elt = State.t
    val pp : Format.formatter -> t -> unit
  end

  val steps : State.t -> Action.t -> StateSet.t
  val steps_list : State.t -> Action.t -> State.t list
  val fold_successors :
    State.t -> 'a -> (Action.t -> State.t -> 'a -> 'a) -> 'a

  val max_repr : State.t -> Action.t -> Action.t

  (** Return a set of symbolic states whose concretizations contain
    * all enabled actions of all concretizations of the given symbolic
    * state. *)
  val enabled_cover : State.t -> ActionSet.t

  val enabled_cover_list : State.t -> Action.t list

  (** [may_be_enabled s a] must return true whenever there
    * exists theta such that [a\theta] is enabled in [s\theta]. *)
  val may_be_enabled : State.t -> Action.t -> bool

  (** [may_be_disabled s a] must return true whenever there
    * exists theta such that [a\theta] is disabled in [s\theta]. *)
  val may_be_disabled : State.t -> Action.t -> bool

  val indep_ee : State.t -> Action.t -> Action.t -> bool
  val indep_de :
    State.t -> Action.t -> Action.t -> [ `Forever_true | `For_now of bool ]

  (** TODO *)
  val first_enabling : Action.t -> State.t -> SemanticActionSet.t option

  (** Representation of sleep sets, which are collections of actions.
    * In the theory, actions are coupled with states, but we ignore states
    * in the implementation since we have no use for them in our
    * instantiation of the functor. *)
  module Z : sig
    include HType
    val empty : t
    val add : Action.t -> t -> t
    val filter_indep : State.t -> Action.t -> t -> t
  end

  (** Type of symbolic actions constrained by sleep sets.
    * This corresponds to A\Z objects in the theory, but they might be
    * represented in a more concise way relying on specificities of
    * the LTS. *)
  module ZAction : sig
    include Printable_simpl
    (** Build an action constrained by a sleep set,
      * return None if the action admits no concretization that
      * is not already in the sleep set. *)
    val make : Action.t -> Z.t -> t option
  end

end

module type Simple = sig

  module State : HType
  module Action : Printable_simpl

  val fold_successors :
    State.t -> 'a -> (Action.t -> State.t -> 'a -> 'a) -> 'a

end

module Make : functor (T:Simple) -> sig
  module StateSet : Set.S with type elt = T.State.t
  val reachable_states : T.State.t -> StateSet.t
  val nb_traces : T.State.t -> int
  val show_traces : ?bound:int -> T.State.t -> unit
  type traces = Traces of (T.Action.t*traces) list
  val traces : T.State.t -> traces
  val display_setTraces : traces -> unit
end
