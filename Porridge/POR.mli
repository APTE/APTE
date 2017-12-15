module Make (S:LTS.S) : sig

  val first_conflicts : S.Action.t -> S.State.t -> S.SemanticActionSet.t
  val first_enabling : S.Action.t -> S.State.t -> S.SemanticActionSet.t
  val stubborn : S.State.t -> S.Action.t -> int -> S.SemanticActionSet.t
  val persistent : S.State.t -> S.ActionSet.t

  module Persistent : LTS.Simple with
    type State.t = S.State.t and
    type Action.t = S.Action.t

  module Sleep : LTS.Simple with
    type State.t = S.State.t*S.Z.t and
    type Action.t = S.ZAction.t

end
