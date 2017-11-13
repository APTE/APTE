module Make (S:LTS.S) : sig

  val first_conflicts : S.ActionSet.elt -> S.State.t -> S.ActionSet.t
  val first_enabling : S.ActionSet.elt -> S.State.t -> S.ActionSet.t
  val stubborn : S.State.t -> S.ActionSet.elt -> S.ActionSet.t
  val persistent : S.State.t -> S.ActionSet.t

  module Persistent : LTS.Simple with type State.t = S.State.t
  module Sleep : sig
    include LTS.Simple
    val from_state : S.State.t -> State.t
  end

end

(** Traces of symbolic actions *)
type action = In of int | Out of int
type tr = Traces of (action * tr) list

(** Traces of symbolic actions *)
val tracesPersistentSleepEquiv : Process.t -> Process.t -> tr
			  
