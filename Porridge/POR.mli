module Make (S:LTS.S) : sig

  val first_conflicts : S.ActionSet.elt -> S.State.t -> S.ActionSet.t
  val first_enabling : S.ActionSet.elt -> S.State.t -> S.ActionSet.t
  val stubborn : S.State.t -> S.ActionSet.elt -> S.ActionSet.t
  val persistent : S.State.t -> S.ActionSet.t

  module Persistent : LTS.Simple with type State.t = S.State.t and type Action.t = S.Action.t

  module Sleep : sig
    include LTS.Simple
    val from_state : S.State.t -> State.t
  end

end
