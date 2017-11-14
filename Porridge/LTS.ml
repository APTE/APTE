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

  (** Symbolic execution. The concrete LTS may be action-deterministic,
    * there is "don't know" non-determinism here due to the symbolic
    * abstraction. *)
  val steps : State.t -> Action.t -> StateSet.t

  (** Fold over all possible successors by enabled actions.
    * [successors s] is the union of all [steps s b] for [b] in
    * [enabled s]. *)
  val fold_successors :
    State.t -> 'a -> (Action.t -> State.t -> 'a -> 'a) -> 'a

  (** Set of symbolic actions that represent all enabled concrete actions of
    * all possible concrete instances of the given symbolic state.
    * This is not necessarily the set of all executable symbolic actions. *)
  val enabled_cover : State.t -> ActionSet.t

  (** [may_be_enabled s a] must return true whenever there
    * exists theta such that [a\theta] is enabled in [s\theta]. *)
  val may_be_enabled : State.t -> Action.t -> bool

  (** [may_be_disabled s a] must return true whenever there
    * exists theta such that [a\theta] is disabled in [s\theta]. *)
  val may_be_disabled : State.t -> Action.t -> bool

  (** [indep_ee s a b] guarantees that all enabled instances of [a] and [b]
    * in [s] are independent. *)
  val indep_ee : State.t -> Action.t -> Action.t -> bool

  (** [indep_de s a b] guarantees that all enabled instances of [a] and [b]
    * in [s] are independent. *)
  val indep_de : State.t -> Action.t -> Action.t -> bool

end

module type Printable = sig
  type t
  val pp : Format.formatter -> t -> unit
  val pp_simpl : Format.formatter -> t -> unit
end

(** Simpler notion of LTS that is not suitable for POR.
  * It is meant for the reduced LTS, which we only need to inspect
  * e.g. to compute statistics. *)
module type Simple = sig

  module State : HType
  module Action : Printable

  (** Fold over all possible successors by enabled actions.
    * [successors s] is the union of all [steps s b] for [b] in
    * [enabled s]. *)
  val fold_successors :
    State.t -> 'a -> (Action.t -> State.t -> 'a -> 'a) -> 'a

end

module Make (T:Simple) = struct

  open T

  module StateSet = Set.Make(State)
  module SMemo = Memo.Make(State)

  (** Set of reachable states from a given state. *)
  let reachable_states = SMemo.make_rec (fun reachable_states s ->
    fold_successors s (StateSet.singleton s)
      (fun a s' res ->
         StateSet.union res (reachable_states s')))

  (** Compute the number of maximal traces of enabled symbolic actions. *)
  let nb_traces = SMemo.make_rec (fun nb_traces s ->
    match
      fold_successors s None
        (fun a s' -> function
           | None -> Some (nb_traces s')
           | Some res -> Some (res + nb_traces s'))
    with
      | None -> 1
      | Some n -> n)

  type traces = Traces of (Action.t*traces) list

  let rec traces s =
    Traces
      (fold_successors s []
         (fun a s' l -> (a, traces s')::l))      
	    
  (** Type of traces, represented in reverse order. *)
  type trace = {
    dest : State.t ;
    prev : (Action.t*trace) option
  }

  let (@) t (a,s) = { dest = s ; prev = Some (a,t) }

  let rec iter_traces f s =
    let rec aux prefix =
      let no_succ =
        fold_successors prefix.dest true
          (fun a s' _ -> aux (prefix @ (a,s')) ; false)
      in
        if no_succ then f prefix
    in aux { dest = s ; prev = None }

  let rec show_trace t =
    match t.prev with
      | None ->
          Format.printf "%a" State.pp t.dest
      | Some (a,t') ->
          show_trace t' ;
          Format.printf
            " -[%a]-> %a"
            Action.pp a
            State.pp t.dest

  let rec show_traces s =
    iter_traces
      (fun t ->
       Format.printf " * " ; show_trace t ; Format.printf "\n")
      s

  (* TODO: use format to create increasing indentation *)
  let rec display_setTraces = function
    | Traces tl ->
       List.iter
	 (fun (act,trs) -> begin
	      Format.printf "%a@," Action.pp_simpl act;
	      Format.printf "->[@[<hov 1>";       
	      display_setTraces trs;
	      Format.printf "] @]@,";
	    end) tl;
       
end
