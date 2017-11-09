module State : LTS.HType
module Action : LTS.HType

module ActionSet : sig
  include Set.S with type elt = Action.t
  val pp : Format.formatter -> t -> unit
end

module StateSet : sig
  include Set.S with type elt = State.t
  val pp : Format.formatter -> t -> unit
end
