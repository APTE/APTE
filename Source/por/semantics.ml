(** Symbolic trace-equivalence LTS for security protocols *)

open Sem_utils

module State = struct

  (** The ghosts are part of the sets of configurations,
    * with special processes bottom_i. *)
  type t = {
    left : Configs.t ;
    right : Configs.t ;
    constraints : Constraints.t
  }

  let compare s1 s2 =
    let c = Configs.compare s1.left s2.left in
      if c <> 0 then c else
        let c = Configs.compare s1.right s2.right in
          if c <> 0 then c else
            Constraints.compare s1.constraints s2.constraints

  let equal t1 t2 = compare t1 t2 = 0

  let hash t =
    Hashtbl.hash (t.left.Configs.id,
                  t.right.Configs.id,
                  Constraints.hash t.constraints)

  let pp ch t =
    Format.fprintf ch "(%a|%a%a)"
      Configs.pp t.left
      Configs.pp t.right
      Constraints.pp t.constraints

end

module Action = struct
  (** Output actions are defined by their channel and a sequence number,
    * which implicitly given the handle w_{c,i} to be used.
    * Input actions are given by their channel and an ordered list
    * of variables. Each variable is indexed by the (sub)frame it
    * refers to. *)
  type t =
    | Out of Channel.t * int
    | In of Channel.t * Term_.invar list
  let equal = (=)
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let pp ch = function
    | Out (c,i) -> Format.fprintf ch "out(%d,%d)" (Channel.to_int c) i
    | In (c,_) -> Format.fprintf ch "in(%d,_)" (Channel.to_int c)
end

module StateSet = struct
  include Set.Make(State)
  let map f s =
    fold (fun elt s -> add (f elt) s) s empty
  let pp ch s =
    Format.pp_print_string ch "{" ;
    iter (fun s -> State.pp ch s ; Format.pp_print_char ch ',') s ;
    Format.pp_print_string ch "}"
end

module ActionSet = struct
  include Set.Make(Action)
  let pp ch s =
    Format.pp_print_string ch "{" ;
    iter (fun s -> Action.pp ch s ; Format.pp_print_char ch ',') s ;
    Format.pp_print_string ch "}"
end

module SMemo = Memo.Make(State)

let list_of f =
  let l = ref [] in
    f (fun x k -> l := x::!l ; k ()) (fun () -> !l)

(** See [split_process]; this is the version for lists, generalized.
  * It takes a function for splitting elements and splits lists. *)
let rec split_list split c l sk fk = match l with
  | [] -> sk ([],c) fk
  | p::l ->
      split c p
        (fun (p',c') k ->
           split_list split c' l
             (fun (l',c'') k ->
                sk (p'::l', c'') k)
             k)
        fk

(** Return a list of processes without toplevel conditionals,
  * together with enriched constraints.
  * This amounts to pull the conditionals at toplevel, by permuting
  * them with plus and par operations. *)
let rec split_process c p sk fk = match p.Process_.contents with
  | Process_.Zero | Process_.Input _ | Process_.Output _ -> sk (p,c) fk
  | Process_.Par l ->
      split_list split_process c l (fun (l,c) k -> sk (Process_.par l, c) k) fk
  | Process_.Plus l ->
      split_list split_process c l (fun (l,c) k -> sk (Process_.plus l, c) k) fk
  | Process_.If (a,b,t,e) ->
      let fk_else () =
        match Constraints.add_neq c a b with
          | Some c -> split_process c e sk fk
          | None -> fk ()
      in
        begin match Constraints.add_eq c a b with
          | Some c -> split_process c t sk fk_else
          | None -> fk_else ()
        end
  | Process_.Bottom _ -> assert false

let split_config c (p,phi) sk fk =
  split_process c p
    (fun (p',c') k -> sk ((p',phi),c') k)
    fk

let split_state s sk fk =
  split_list split_config s.State.constraints (Configs.to_list s.State.left)
    (fun (l',c') k ->
       split_list split_config c' (Configs.to_list s.State.right)
         (fun (r',c'') k ->
            sk { State.
                 left = Configs.of_list l' ;
                 right = Configs.of_list r' ;
                 constraints = c'' } k)
         k)
    fk

let () =
  Check.add_suite
    ("Semantics",
     [ "Size of split_process", `Quick,
       (fun () ->
          let u = Term_.var "u" in
          let v = Term_.var "v" in
          let w = Term_.var "w" in
          let c = Channel.of_int 0 in
          let d = Channel.of_int 1 in
          let p = Process_.input c (Term_.var "x") Process_.zero in
          let q = Process_.input d (Term_.var "x") Process_.zero in
          let r = Process_.output c u Process_.zero in
          let s = Process_.output c v Process_.zero in
          let cstr = Constraints.empty in
          let count p = List.length (list_of (split_process cstr p)) in
            Alcotest.(check int) "nb of splits for (if u=u then P else Q)"
              1
              (count (Process_.if_eq u u p q)) ;
            Alcotest.(check int) "nb of splits for (if u=v then P else Q)"
              2
              (count (Process_.if_eq u v p q)) ;
            Alcotest.(check int) "nb of splits for (if v=u then P else Q)"
              2
              (count (Process_.if_eq v u p q)) ;
            Alcotest.(check int) "nb of splits for (if v=u then P else Q | if u=v then R else S)"
              2
              (count (Process_.par [Process_.if_eq v u p q; Process_.if_eq u v r s])) ;
            Alcotest.(check int) "nb of splits for (if u=v then (if v=w then P else R) else Q)"
              3
              (count (Process_.if_eq u v (Process_.if_eq v w p r) q)) ;
            Alcotest.(check int) "nb of splits for (if v=u then P else Q | if u=w then R else S)"
              4
              (count (Process_.par [Process_.if_eq v u p q; Process_.if_eq u w r s])) ;
            Alcotest.(check int) "nb of splits for (if v=u then P else 0 | if u=w then R else S)"
              4
              (count (Process_.par [Process_.if_eq v u p Process_.zero; Process_.if_eq u w r s])) ;
            Alcotest.(check int) "nb of splits for (if v=u then P else Q + if u=w then R else S)"
              4
              (count (Process_.plus [Process_.if_eq v u p q; Process_.if_eq u w r s])) ;
       ) ;
     ] )

        (*
let alive_frames s =
  let alive =
    List.map snd
      (List.filter
         (fun (p,phi) ->
            match p.Process_.contents with
              | Process_.Bottom _ -> false
              | _ -> true)
         (List.rev_append t.left t.right))
  in
    List.sort_uniq Frame.compare alive

let age s =
  let dead =
    List.filter
      (fun (p,phi) ->
         match p.Process_.contents with
           | Process_.Bottom _ -> true
           | _ -> false)
      (List.rev_append t.left t.right)
  in
    (* TODO check that duplicates can't occur or don't matter *)
    List.length dead

let post_process s =
  let left = List.map Process_.compute_quiescents s.left in
  let right = List.map Process_.compute_quiescents s.right in

(** Source of fresh input variables.
  * We could also use unique identifiers of configurations
  * but we don't have them at least for now. *)
let fresh =
  let c = Array.make Channel.nb_chan 0 in
    fun i ->
      c.(i) <- 1+c.(i) ;
      c.(i)

let pre_transitions = SMemo.make (fun s ->
  let left_tables =
    List.map
      (fun (p,phi) -> SymProc.transitions p, phi)
      s.left
  in
  let right_tables =
    List.map
      (fun (p,phi) -> SymProc.transitions p, phi)
      s.right
  in
  let age = age s in
  (** The set of configurations [results] will be extended
    * by successively incorporating the possible outcomes of all initial
    * configurations.
    * The incorporation process may duplicate the current results
    * into two copies with incompatible constraints. *)
  let results = ref
    [ { left = Configs.empty ; right = Configs.empty ;
        constraints = s.constraints } ]
  in
  let incorporate results table phi side =
    let alternatives = get_action table in
      if alternatives = [] then
        List.map (fun s -> States.insert side (Process_.bottom age)) results
      else


          Configs.map (fun c ->
                         Configs.add (symproc,phi) results
          else
            List.fold_left
              (fun results f ->
                 let symproc = f (Term_.invar (c,phi.Frame.id,n)) in
                   Configs.add (symproc,phi) results)
              results
              table.input.(i)
    in
      { left = aggregate Configs.empty left_tables ;
        right = aggregate Configs.empty right_tables }
  in
      assert false

         *)
                 (*
let enabled = SMemo.make_rec (fun enabled (p,i) ->
  match p.Process_.contents with
  | Process_.Zero -> ActionSet.empty
  | Process_.Input (c,_,_) -> ActionSet.singleton (Action.Input (c,i))
  | Process_.Output (c,_,_) -> ActionSet.singleton (Action.Output c)
  | Process_.Par l | Process_.Plus l ->
      List.fold_left
        (fun s p -> ActionSet.union s (enabled (p,i)))
        ActionSet.empty
        l)

(** Compute successor states
  * as a mapping from actions to sets of processes. *)
let successors = SMemo.make_rec (fun successors (p,i) ->
  match p.Process_.contents with
  | Process_.Zero -> ActionMap.empty
  | Process_.Input (c,_,p) ->
      ActionMap.singleton (Action.Input (c,i)) (StateSet.singleton (p,i))
  | Process_.Output (c,_,p) ->
      ActionMap.singleton (Action.Output c) (StateSet.singleton (p,i+1))
  | Process_.Plus l ->
      List.fold_left
        (ActionMap.merge
           (fun _ x y -> match x,y with
              | None, x | x, None -> x
              | Some l, Some l' -> Some (StateSet.union l l')))
        ActionMap.empty
        (List.map (fun p -> successors (p,i)) l)
  | Process_.Par l ->
      let rec aux acc ll l : StateSet.t ActionMap.t list = match l with
        | p::l ->
            let m = successors (p,i) in
            let add_par (q,j) =
              (Process_.par (List.rev_append ll (q::l)),j)
            in
            let m = ActionMap.map (StateSet.map add_par) m in
              aux (m::acc) (p::ll) l
        | [] -> acc
      in
        List.fold_left
          (ActionMap.merge
             (fun _ x y -> match x,y with
                | None, x | x, None -> x
                | Some l, Some l' -> Some (StateSet.union l l')))
          ActionMap.empty
          (aux [] [] l))

  (** Check that two actions are independent in a given state.
    * We follow the definition of a valid independence relation but
    * allow the use of a domain-specific force_dependent function,
    * that is typically used to reflect dependences that would only
    * appear in concretizations of actions and states. *)
  let independent s a b = (* TODO memoize *)
    (not (force_dependent a b)) && (* TODO remove: useful only when both
     * enabled, in which case we don't want to force anything since only
     * restricted inputs are enabled in concurrency with outputs
     *
     * we need In(c,0) and In(c,1) to be dependent... it seems fragile to
     * declare them dependent when they are not simultaneously enabled,
     * i.e. with frame size 0 or 1 *)
    let succ = successors s in
    let find m x = try Some (ActionMap.find x m) with Not_found -> None in
      match find succ a, find succ b with
        | None, None -> true
        | Some sa, Some sb ->
            let a = translate a b in
            let b = translate b a in
            begin match find (successors sa) b, find (successors sb) a with
              | Some sab, Some sba -> State.equal sab sba
              | _ -> false
            end
        | _ -> false
                  *)
