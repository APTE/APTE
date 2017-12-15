open Sem_utils
open Frame

module State : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val make :
    left:Configs.t ->
    right:Configs.t ->
    constraints:Constraints.t ->
    inputs:(int Channel.Map.t) ->
    t
  val of_process :
    ?constraints:Constraints.t ->
    ?inputs:(int Channel.Map.t) ->
    Process.t ->
    t
  val empty : t
  val update :
    ?left:Configs.t ->
    ?right:Configs.t ->
    ?constraints:Constraints.t ->
    ?inputs:(int Channel.Map.t) ->
    t -> t
end
module Action : sig
  type t =
    | Out of Channel.t * int
    | In of Channel.t * int * Domain.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val pp_simpl : Format.formatter -> t -> unit (* compact pretty printing *)
end

module SemanticAction : LTS.HType_simpl with type t = Action.t

module ActionSet : sig
  include Set.S with type elt = Action.t
  val pp : Format.formatter -> t -> unit
end

module SemanticActionSet : sig
  type t
  type elt = Action.t
  val equal : t -> t -> bool
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

val enabled_cover : State.t -> ActionSet.t
val enabled_cover_list : State.t -> Action.t list
val max_repr : State.t -> Action.t -> Action.t

val may_be_enabled : State.t -> Action.t -> bool
val may_be_disabled : State.t -> Action.t -> bool

val indep_ee : State.t -> Action.t -> Action.t -> bool
val indep_de :
  State.t -> Action.t -> Action.t -> [ `Forever_true | `For_now of bool ]

val first_enabling : Action.t -> State.t -> SemanticActionSet.t option

module Z : sig
  include LTS.HType
  val empty : t
  val add : Action.t -> t -> t
    val filter_indep : State.t -> Action.t -> t -> t
end

module ZAction : sig
  type t =
    | Output of Channel.t
    | Input of Channel.t * Domain.t * Domain.t option
  val pp : Format.formatter -> t -> unit
  val pp_simpl : Format.formatter -> t -> unit
  val make : Action.t -> Z.t -> t option
end
