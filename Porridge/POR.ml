open LTS

module Debug = struct
  let fc = false
  let fe = false
  let stubborn = false
  let stubborn_steps = false
end

module Make (T:S) = struct

  open T

  module SMemo = Memo.Make(State)
  module AMemo = Memo.Make(SemanticAction)

  (** Return the set of first conflicting actions wrt a given initial state
    * and enabled action. More precisely, we are looking for actions b such that:
    *   there exists a trace t from s to s',
    *   in which all actions are independent with a,
    *   and such that a and b are dependent in s',
    * We require that [a] can be executed in [s] (thus also in [s']).
    * However, the action does not need to belong to [enabled s]. *)
  let first_conflicts =
    AMemo.make_rec (fun fc a -> SMemo.make (fun s ->
      (* It is correct to return [fc (max_repr s a) s] for [fc a s]
       * and it can save significant computations. In theory it could
       * lead to less precise first_conflicts sets, but for Trace_equivalence
       * I do not see when this could make a difference! TODO *)
      let a' = max_repr s a in
      if not (SemanticAction.equal a' a) then fc a' s else
      fold_successors s SemanticActionSet.empty
        (fun b s' res ->
           if indep_ee s a b then
             let fc' = fc a s' in
               if Debug.fc then
               Format.printf "@[<2>fc(@[%a,@,%a@])=@,%a@]@."
                 State.pp s'
                 Action.pp a
                 SemanticActionSet.pp fc' ;
               SemanticActionSet.union res fc'
           else
             SemanticActionSet.add b res)))

  (** First enabling actions.
    * The given action is not enabled in the given state. *)
  let first_enabling =
    AMemo.make_rec (fun fe a -> SMemo.make (fun s ->
      match T.first_enabling a s with Some s -> s | None ->
      assert (may_be_disabled s a) ;
      fold_successors s SemanticActionSet.empty
        (fun b s' res ->
           match indep_de s a b with
             | `For_now true ->
                 let fe' = fe a s' in
                   if Debug.fe then
                     Format.printf "@[<2>fe(@[%a,@,%a@])=@,%a@]@."
                       State.pp s'
                       Action.pp a
                       SemanticActionSet.pp fe' ;
                   SemanticActionSet.union res fe'
             | `Forever_true -> res
             | `For_now false ->
                 SemanticActionSet.add b res)))

  exception Suboptimal

  (** [stubborn s a] computes the enabled part of a (conditional) stubborn set
    * for some state [s] and seed action [a].
    * Always returns sets of cardinal stricly less than [n], raising
    * [Suboptimal] if the normal computation of the set would violate this
    * condition. *)
  let stubborn s a n =
    (* Note: to filter the enabled part of the stubborn set being computed,
     * we use may_be_enabled which does exactly the job, but is costly.
     * An alternative is to intersect with enabled_cover seen as a
     * SemanticActionSet. This seems correct for Trace_equivalence because we
     * always obtain sets in which maximum representatives are present, but
     * it's not clear that this would always be the case. *)
    let rec aux stable = function
      | [] -> stable
      | aset::frontier ->
          if SemanticActionSet.is_empty aset then
            aux stable frontier
          else
            let a,aset = SemanticActionSet.choose aset in
            let frontier = aset::frontier in
              if SemanticActionSet.mem a stable then
                aux stable frontier
              else
                let may_be_enabled = may_be_enabled s a in
                let stable =
                  if not may_be_enabled then stable else
                    if SemanticActionSet.cardinal stable < n-1 then
                      SemanticActionSet.add a stable
                    else
                      raise Suboptimal
                in
                let frontier =
                  if may_be_enabled then
                    let fc = first_conflicts a s in
                      if Debug.fc then
                        Format.printf
                          "@[<2>fc(@[%a,@,%a@]) =@ %a@]@."
                          State.pp s
                          Action.pp a
                          SemanticActionSet.pp fc ;
                      fc::frontier
                   else
                     frontier
                in
                let frontier =
                  if may_be_disabled s a then
                    let fe = first_enabling a s in
                      if Debug.fe then
                        Format.printf
                          "@[<2>fe(@[%a,@,%a@]) =@ %a@]@."
                          State.pp s
                          Action.pp a
                          SemanticActionSet.pp fe ;
                      fe::frontier
                  else frontier
                in
                  aux stable frontier
    in
      aux SemanticActionSet.empty [SemanticActionSet.singleton a]

  (** Breadth-first stubborn set computation
    *
    * We use a priority queue with states as values.
    * Keys are composed of:
    *
    *   - the set of actions for which first conflicts need
    *     to be computed;
    *
    *   - the depth of the state in the exploration, i.e. the
    *     minimum number of actions necessary to reach it;
    *
    *   - a list of sets corresponding to the traces (without
    *     taking the order into account) that have been explored
    *     to reach that state.
    *
    * The second component is used to prioritize computations.
    * The third component is used to dismiss nodes when additions
    * to the current set make it useless to keep exploring a
    * state. *)

  module Keys = struct
    type t = int * SemanticActionSet.t * ActionSet.t list
    let compare : t -> t -> int =
      fun (n,_,_) (m,_,_) -> Pervasives.compare n m
    let dummy = -1, SemanticActionSet.empty, []
  end
  module Priority = Priority.Make(Keys)
  module SH = Hashtbl.Make(State)

  let indep_xe s a b =
    let ind_e = if may_be_enabled s a then indep_ee s a b else true in
      if ind_e then
        if may_be_disabled s a then indep_de s a b else `For_now true
      else
        `For_now false

  let indep_xe s set b =
    SemanticActionSet.fold
      (fun a res ->
         match res with
           | `For_now false -> `For_now false
           | `Forever_true -> indep_xe s a b
           | `For_now true ->
               let i = indep_xe s a b in
                 if i = `Forever_true then res else i)
      set
      `Forever_true

  type current = {
    enabled : SemanticActionSet.t ;
    other : SemanticActionSet.t ;
    enabled_size : int
  }

  let stubborn s0 a max_size =
    (** [q] is the priority queue, containing at most one node per state.
      * [nodes] allows to associate a state to a node in the queue that
      * has the state as its value, if there exists one. *)
    let nodes = SH.create 257 in
    let q = Priority.create 100 Keys.dummy s0 in
    (** [current] is the current (partial) stubborn set.
      * It contains all actions that are found as keys in
      * the priority queue. *)
    let rec aux current =
      if Priority.size q = 0 then current else
        let node = Priority.extract_min q in
        let nb_actions,actions,traces = Priority.key node in
        let s = Priority.value node in
        SH.remove nodes s ;
        if Debug.stubborn_steps then
          Format.printf "@[<2>%d:@ %a@ %a@]@."
            nb_actions
            SemanticActionSet.pp actions
            State.pp s ;
        (* TODO cut based on traces *)
        let transitions = enabled_cover_list s in
        let transitions =
          List.filter
            (fun a ->
               not (SemanticActionSet.mem a current.enabled) &&
               not (SemanticActionSet.mem a current.other))
            transitions
        in
        (* Function for exploring further independent transitions *)
        let explore a s' =
          if Debug.stubborn_steps then
            Format.printf "exploring along %a@." Action.pp a ;
          let traces =
            List.map (fun set -> ActionSet.add a set) traces
          in
          try
            let node = SH.find nodes s' in
            let (m,a,l) = Priority.key node in
            let key' =
              min m (nb_actions+1),
              SemanticActionSet.union a actions,
              List.rev_append traces l
            in
              Priority.decrease_key q node key'
          with Not_found ->
            let node = Priority.insert q (nb_actions+1,actions,traces) s' in
              SH.add nodes s' node
        in
        (** Process all (relevant) transitions, updating the stubborn
          * set with dependent actions. *)
        let process to_add a =
          match indep_xe s actions a with
            | `Forever_true ->
                Format.printf "INFO: useless explo. aborted@." ;
                to_add
            | `For_now true ->
                List.iter (fun s' -> explore a s') (steps_list s a) ;
                to_add
            | `For_now false ->
                a::to_add
        in
        let to_add = List.fold_left process [] transitions in
        let current =
          List.fold_left
            (fun current a ->
               if Debug.stubborn_steps then
                 Format.printf "to_add %a@." Action.pp a ;
               if may_be_enabled s0 a then
                 let s = SemanticActionSet.add a current.enabled in
                 let new_size = SemanticActionSet.cardinal s in
                   if new_size >= max_size then begin
                     if Debug.stubborn_steps then
                       Format.printf "Suboptimal: %a+%a@."
                         SemanticActionSet.pp current.enabled
                         Action.pp a ;
                     raise Suboptimal
                   end else
                     { current with
                         enabled = s ;
                         enabled_size = new_size }
               else
                 { current with other = SemanticActionSet.add a current.other })
            current
            to_add
        in
          if to_add <> [] then begin
            if Debug.stubborn_steps then
              Format.printf "current = %a, %a@."
                SemanticActionSet.pp current.enabled SemanticActionSet.pp current.other ;
            let key =
              0,
              List.fold_left
                (fun set a -> SemanticActionSet.add a set)
                SemanticActionSet.empty
                to_add,
              [ActionSet.empty]
            in
            let node = Priority.insert q key s0 in
              SH.add nodes s node
          end ;
          aux current
    in
    let init_set = SemanticActionSet.singleton a in
    let node =
      Priority.insert q (0,init_set,[ActionSet.empty]) s0
    in
      SH.add nodes s0 node ;
      (aux { enabled_size = 1 ; enabled = init_set ;
             other = SemanticActionSet.empty }).enabled

  (** Compute minimal non-empty persistent sets from stubborn sets. *)
  let persistent : State.t -> ActionSet.t = SMemo.make (fun s ->
    let enabled = enabled_cover s in
    let senabled =
      ActionSet.fold SemanticActionSet.add (enabled_cover s) SemanticActionSet.empty
    in
    let sset,_ =
    ActionSet.fold
      (fun a (res,n) ->
         if n = 1 then res,n else try
           let res' = stubborn s a n in
             if Debug.stubborn then
               Format.printf
                 "@[<2>P(%a,%a)=@,%a@]@."
                 State.pp s
                 Action.pp a
                 SemanticActionSet.pp res' ;
             (res',SemanticActionSet.cardinal res')
         with
           | Suboptimal -> res,n)
      enabled
      (senabled, SemanticActionSet.cardinal senabled)
    in SemanticActionSet.fold (fun a s -> ActionSet.add a s) sset ActionSet.empty)

  (** Persistent restriction of the semantics [S]. *)
  module Persistent :
         LTS.Simple with type State.t = State.t and type Action.t = T.Action.t
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

  (** Functor for building the sleep LTS.
    *
    * TODO another version keeping only minimal sleep sets to avoid having
    * more states, at the cost of more traces... we need to investigate
    * which version would be best depending on applications. *)
  module Sleep : LTS.Simple with
    type State.t = State.t*Z.t and
    type Action.t = ZAction.t
  = struct

    module State = struct
      type t = State.t * Z.t
      let pp ch (s,z) =
        Format.fprintf ch "@[<hov>%a@,%a@]"
          State.pp s
          Z.pp z
      let equal (s,z) (s',z') =
        State.equal s s' && Z.equal z z'
      let compare (s,z) (s',z') =
        let c = State.compare s s' in
          if c <> 0 then c else
            Z.compare z z'
      let hash (s,z) = Hashtbl.hash (State.hash s, Z.hash z)
    end

    module Action = ZAction

    let fold_successors ((s,z):State.t) x f =
      let rec fold x = function
        | [] -> x
        | a::more ->
            match ZAction.make a z with None -> x | Some az ->
            let x =
              StateSet.fold
                (fun s' x ->
                   (* Updating actions present in initial sleep set. *)
                   let update_z = Z.filter_indep s a z in
                   (* New additions to the sleep set. *)
                   let rec new_z acc = function
                     | [] -> acc
                     | b::l ->
                         if indep_ee s a b then
                           new_z (Z.add b acc) l
                         else
                           new_z acc l
                   in
                     f az (s', new_z update_z more) x)
                (steps s a)
                x
            in
              fold x more
      in
        fold x (ActionSet.elements (persistent s))

  end

end
