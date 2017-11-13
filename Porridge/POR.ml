open LTS

module Make (T:S) = struct

  open T

  module SMemo = Memo.Make(State)
  module AMemo = Memo.Make(Action)

  (** Return the set of first conflicting actions wrt a given initial state
    * and enabled action. More precisely, we are looking for actions b such that:
    *   there exists a trace t from s to s',
    *   in which all actions are independent with a,
    *   and such that a and b are dependent in s',
    * We require that [a] can be executed in [s] (thus also in [s']).
    * However, the action does not need to belong to [enabled s]. *)
  let first_conflicts =
    AMemo.make_rec (fun fc a -> SMemo.make (fun s ->
      fold_successors s ActionSet.empty
        (fun b s' res ->
           if indep_ee s a b then
             let fc' = fc a s' in
               if Debug.first then
               Format.printf "fc(%a,%a)=%a\n"
                 State.pp s'
                 Action.pp a
                 ActionSet.pp fc' ;
               ActionSet.union res fc'
           else
             ActionSet.add b res)))

  (** First enabling actions.
    * The given action is not enabled in the given state. *)
  let first_enabling =
    AMemo.make_rec (fun fe a -> SMemo.make (fun s ->
      fold_successors s ActionSet.empty
        (fun b s' res ->
           if indep_de s a b then
             let fe' = fe a s' in
               if Debug.first then
               Format.printf "fe(%a,%a)=%a\n"
                 State.pp s'
                 Action.pp a
                 ActionSet.pp fe' ;
               ActionSet.union res fe'
           else
             ActionSet.add b res)))

  (** Compute the (conditional) stubborn set for some state,
    * given a seed action that should be enabled in that state. *)
  let stubborn s a =
    (* TODO
     *   - stop when all enabled actions have been added
     *   - domain-specific acceleration: add reachable outputs
     *     as soon as an input is present *)
    let rec aux stable = function
      | [] -> stable
      | aset::frontier ->
          if ActionSet.is_empty aset then
            aux stable frontier
          else
            let a = ActionSet.choose aset in
            let aset = ActionSet.remove a aset in
            let frontier = aset::frontier in
              if ActionSet.mem a stable then
                aux stable frontier
              else
                let frontier =
                  if may_be_enabled s a then
                    let fc = first_conflicts a s in
                      if Debug.first then
                        Format.printf
                          "fc(%a,%a) = %a\n"
                          State.pp s
                          Action.pp a
                          ActionSet.pp fc ;
                      fc::frontier
                   else
                     frontier
                in
                let frontier =
                  if may_be_disabled s a then
                    let fe = first_enabling a s in
                      if Debug.first then
                        Format.printf
                          "fe(%a,%a) = %a\n"
                          State.pp s
                          Action.pp a
                          ActionSet.pp fe ;
                      fe::frontier
                  else
                    frontier
                in
                  aux (ActionSet.add a stable) frontier
    in
      aux ActionSet.empty [ActionSet.singleton a]

  (** Compute minimal non-empty persistent sets from stubborn sets. *)
  let persistent : State.t -> ActionSet.t = SMemo.make (fun s ->
    (* TODO
     *   - stop when singleton found
     *   - domain-specific heuristic: start with outputs *)
    let enabled = enabled_cover s in
    ActionSet.fold
      (fun a res ->
         let ssa = stubborn s a in
         let res' = ActionSet.inter enabled ssa in
           if Debug.stubborn then
           Format.printf
             "S(%a,%a)=%a\n =E(%a)\n"
             State.pp s
             Action.pp a
             ActionSet.pp res'
             ActionSet.pp ssa ;
           if ActionSet.is_empty res ||
              ActionSet.cardinal res' < ActionSet.cardinal res
           then res' else res)
      enabled
      ActionSet.empty)

  (** Persistent restriction of the semantics [S]. *)
  module Persistent :
         LTS.Simple with type State.t = State.t and type Action.t = Action.t
  = struct

    module State = State
    module Action = Action

    let fold_successors s x f =
      ActionSet.fold
        (fun a x ->
           StateSet.fold
             (fun s' x -> f a s' x)
             (steps s a)
             x)
        (persistent s)
        x

  end

  (** Functor for building the abstract sleep LTS,
    * where traces are lexicographic minima.
    *
    * A state may be encountered with multiple sleep sets so this LTS may
    * have more states than the original one. However it has less traces.
    *
    * TODO another version keeping only minimal sleep sets to avoid having
    * more states, at the cost of more traces... we need to investigate
    * which version would be best depending on applications
    *
    * XXX it would make sense to build Sleep(S) for any LTS S,
    *   but we are mainly interested in Sleep(Persistent(S)) and the abstract
    *   construction would only work here if the notion of independence taken
    *   for Persistent(S) is that of S and not the natural one for
    *   Persistent(S). *)
  module Sleep : sig
    include LTS.Simple
    val from_state : T.State.t -> State.t
  end = struct

    module ActionMap = Map.Make(Action)
    let pp_z ch m = Format.fprintf ch "#z=%d" (ActionMap.cardinal m)

    (** States are decorated with a sleep set that forbids some actions.
      * More specifically it is a map from actions to states, indicating
      * that for any action in the domain of the map, the enabled instances
      * of the action in (any of) the associated states have been put to
      * sleep. *)
    type z = State.t ActionMap.t

    module State = struct
      type t = State.t * z
      let pp ch (s,z) =
        Format.fprintf ch "%a\\%a"
          State.pp s
          pp_z z
      let equal (s,z) (s',z') =
        State.equal s s' && ActionMap.equal State.equal z z'
      let compare (s,z) (s',z') =
        let c = State.compare s s' in
          if c <> 0 then c else
            ActionMap.compare State.compare z z'
      let hash x = assert false ; Hashtbl.hash x (* TODO FIXME incorrect! *)
      end

    let from_state s = s,ActionMap.empty

    module Action = struct
      type t = Action.t * z
      let pp ch (a,z) =
        Format.fprintf ch "%a\\%a"
          Action.pp a
          pp_z z
    end

    let fold_successors ((s,z):State.t) x f =
      let rec fold x = function
        | [] -> x
        | a::more ->
            StateSet.fold
              (fun s' x ->
                 (* Updating actions present in initial sleep set. *)
                 let update_z =
                   ActionMap.filter
                     (fun b s_b -> indep_ee s a b)
                     z
                 in
                 (* New additions to the sleep set. *)
                 let rec new_z acc = function
                   | [] -> acc
                   | b::l ->
                       if indep_ee s a b then
                         new_z (ActionMap.add b s acc) l
                       else
                         new_z acc l
                 in
                   f (a,z) (s', new_z update_z more) x)
              (steps s a)
              x
      in
        fold x (ActionSet.elements (persistent s))

  end

end

(* New functions for easing interfaces with Apte *)
module POR = Make(Trace_equiv)
module Persistent = POR.Persistent
module RedLTS = LTS.Make(Persistent)

(** Traces of symbolic actions *)
type action = In of int | Out of int
type tr = Traces of (action * tr) list

let make_state p1 p2 =
  Trace_equiv.State.make
    ~left:(Sem_utils.Configs.of_process p1)
    ~right:(Sem_utils.Configs.of_process p2)
    ~constraints:Sem_utils.Constraints.empty
    
let simplAction = function
  | Trace_equiv.Action.Out (ch,_) -> Out (Channel.to_int ch)
  | Trace_equiv.Action.In (ch,_) -> In (Channel.to_int ch)
				      
let rec simpleTraces  = function
  | RedLTS.Traces tl ->
     Traces (List.map (fun (act, trs) -> (simplAction act, simpleTraces trs)) tl)
	      
let tracesPersistentSleepEquiv p1 p2 =
  let sinit = make_state p1 p2 in
  let trLTS = RedLTS.traces sinit in
  simpleTraces trLTS
	      
