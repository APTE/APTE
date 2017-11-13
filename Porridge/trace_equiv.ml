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
    Format.fprintf ch "(%a≈%a%a)"
      Configs.pp t.left
      Configs.pp t.right
      Constraints.pp t.constraints

  let config_list s =
    List.rev_append
      s.left.Configs.contents
      s.right.Configs.contents

  let alive_frames s =
    let alive =
      List.map snd
        (List.filter
           (fun (p,phi) ->
              match p.Process.contents with
                | Process.Bottom _ -> false
                | _ -> true)
           (config_list s))
  in
    List.sort_uniq Frame.compare alive

  let age s =
    let dead =
      List.filter
        (fun (p,phi) ->
           match p.Process.contents with
             | Process.Bottom _ -> true
             | _ -> false)
        (config_list s)
    in
      (* TODO check that duplicates can't occur or don't matter *)
      List.length dead

  let frame_size ?channel s =
    match alive_frames s with
      | [] -> assert false
      | phi::_ ->
          match channel with
            | None -> Frame.size phi
            | Some c -> Frame.size_on_channel phi c

end

module Action = struct
  (** Output actions are defined by their channel and a sequence number,
    * which implicitly given the handle w_{c,i} to be used.
    * Input actions are given by their channel and an ordered list
    * of variables. Each variable is indexed by the (sub)frame it
    * refers to. *)
  type t =
    | Out of Channel.t * int
    | In of Channel.t * Term.invar list
  let equal = (=)
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let pp ch = function
    | Out (c,i) -> Format.fprintf ch "out(%c,%d)" (Channel.to_char c) i
    | In (c,l) ->
        Format.fprintf ch "in(%c,[%a])"
          (Channel.to_char c)
          (Format.pp_print_list
             ~pp_sep:(fun ch () -> Format.pp_print_char ch ',')
             Term.pp)
          (List.map (fun (x,y,z) -> Term.invar x y z) l)
  let same_skel a b = match a,b with
    | Out (c,_), Out (d,_)
    | In (c,_), In (d,_) when c = d -> true
    | _ -> false
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
    Format.pp_print_list
      ~pp_sep:(fun ch () -> Format.pp_print_char ch ',')
      Action.pp
      ch (elements s) ;
    Format.pp_print_string ch "}"
end

module SMemo = Memo.Make(State)

module SuccFail = struct

  let list_of f =
    let l = ref [] in
      f (fun x k -> l := x::!l ; k ()) (fun () -> !l)

  let fold c f x =
    let x = ref x in
      c (fun v k -> x := f !x v ; k ()) (fun () -> !x)

  let stateset_of f =
    fold f (fun s v -> StateSet.add v s) StateSet.empty

end

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
let rec split_process c p sk fk = match p.Process.contents with
  | Process.Zero | Process.Input _ | Process.Output _ | Process.Bottom _ -> sk (p,c) fk
  | Process.Par l ->
      split_list split_process c l (fun (l,c) k -> sk (Process.par l, c) k) fk
  | Process.Plus l ->
      split_list split_process c l (fun (l,c) k -> sk (Process.plus l, c) k) fk
  | Process.If (a,b,t,e) ->
      let fk_else () =
        match Constraints.add_neq c a b with
          | Some c -> split_process c e sk fk
          | None -> fk ()
      in
        begin match Constraints.add_eq c a b with
          | Some c -> split_process c t sk fk_else
          | None -> fk_else ()
        end

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

(** Source of fresh input variables.
  * We could also use unique identifiers of configurations
  * but we don't have them at least for now. *)
let fresh =
  let c = Array.make Channel.nb_chan 0 in
    fun i ->
      c.(i) <- 1+c.(i) ;
      c.(i)

let is_ghost p = match p.Process.contents with
  | Process.Bottom _ -> true
  | _ -> false

type 'a trans_table = {
  output : (Action.t * (State.t list)) array ;
  input : (Action.t * (State.t list)) array
}

let config_tables = SMemo.make (fun s ->
  let left_tables =
    List.map
      (fun (p,phi) ->
        if is_ghost p then
          `Ghost (p,phi)
        else
          `Table (Process.transitions p, phi))
      s.State.left.Configs.contents
  in
  let right_tables =
    List.map
      (fun (p,phi) ->
         if is_ghost p then
           `Ghost (p,phi)
         else
           `Table (Process.transitions p, phi))
      s.State.right.Configs.contents
  in
    left_tables, right_tables)

let pre_steps ~get_action_entries ~config_of_entry s =
  let age = State.age s in
  let ghost phi = (Process.bottom age, phi) in
  let left_tables, right_tables = config_tables s in
  let configs tables =
    List.fold_left
      (fun res -> function
         | `Ghost c -> Configs.add c res
         | `Table (table,phi) ->
             let entries = get_action_entries table in
               if entries = [] then
                 Configs.add (ghost phi) res
               else
                 List.fold_left
                   (fun res entry ->
                      Configs.add (config_of_entry entry phi) res)
                   res
                   entries)
      Configs.empty
      tables
  in
  let s = { State.
            left = configs left_tables ;
            right = configs right_tables ;
            constraints = s.State.constraints } in
    if State.alive_frames s = [] then
      fun sk fk -> fk ()
    else
      (** Split conditionals,
        * enriching constraints as by-product. *)
      split_state s

let steps s a =
  match a with
    | Action.Out (c,n) ->
        if n <> State.frame_size ~channel:c s then StateSet.empty else
        let get_action_entries table =
          table.Process.output.(Channel.to_int c)
        in
        let config_of_entry (t,p) phi =
          p, Frame.append phi c t
        in
          SuccFail.stateset_of
            (pre_steps ~get_action_entries ~config_of_entry s)
    | Action.In (c,vars) ->
        try
        let get_action_entries table =
          table.Process.input.(Channel.to_int c)
        in
        let config_of_entry k phi =
          let d,id,n =
            List.find
              (fun (_,id,_) ->
                 let psi = Frame.of_id id in
                   Frame.prefix psi phi)
              vars
          in
            assert (d = c) ;
            k (Term.invar c id n), phi
        in
          SuccFail.stateset_of
            (pre_steps ~get_action_entries ~config_of_entry s)
        with Not_found -> StateSet.empty

let transitions = SMemo.make (fun s ->
  (** Given a function for accessing the processes' transition table
    * entries, and transforming entries into resulting configurations,
    * build the resulting Configs.t. *)
  let configs_of_tables make_action config_of_entry get_action_entries =
    Array.init
      Channel.nb_chan
      (fun c ->
         let config_of_entry = config_of_entry c in
         let get_action_entries = get_action_entries c in
         let res = pre_steps ~config_of_entry ~get_action_entries s in
         let statelist = SuccFail.list_of res in
           make_action c,
           statelist)
  in
  let outputs =
    configs_of_tables
      (fun c ->
         let c = Channel.of_int c in
           Action.Out (c, State.frame_size ~channel:c s))
      (fun c (t,p) phi -> p, Frame.append phi (Channel.of_int c) t)
      (fun c table -> table.Process.output.(c))
  in
  let input_uid : int array = Array.init Channel.nb_chan fresh in
  let h = Hashtbl.create 17 in
  let create_var c phi =
    let iv = (Channel.of_int c, phi.Frame.id, input_uid.(c)) in
    let v = Term.invar (Channel.of_int c) phi.Frame.id input_uid.(c) in
      Hashtbl.replace h (c,phi.Frame.id) iv ;
      v
  in
  let get_vars c =
    Hashtbl.fold
      (fun (c',phi) v l -> if c=c' then v::l else l)
      h []
  in
  let inputs =
    configs_of_tables
      (fun c -> Action.In (Channel.of_int c, get_vars c))
      (fun c k phi ->
         let v = create_var c phi in
           k v, phi)
      (fun c table -> table.Process.input.(c))
  in
    { output = outputs ; input = inputs })

let enabled_cover s =
  let tables = transitions s in
  let enabled arr s0 =
    Array.fold_left
      (fun s (a,l) ->
        if l = [] then s else ActionSet.add a s)
      s0
      arr
  in
    enabled tables.output (enabled tables.input ActionSet.empty)

let fold_successors s x f =
  let tables = transitions s in
  let fold arr x =
    Array.fold_left
      (fun x (a,l) ->
         List.fold_left
           (fun x s' -> f a s' x)
           x
           l)
      x
      arr
  in
    fold tables.output (fold tables.input x)

(** If an action can be executed symbolically it can also be executed
  * concretely, provided that constraints are feasible.
  * Thus we return that any symbolically executable action may have
  * concretely enabled instances. *)
let may_be_enabled s a =
  let tables = transitions s in
    match a with
      | Action.Out (c,_) ->
          let (a',l) = tables.output.(Channel.to_int c) in
            a = a' && l <> []
      | Action.In (c,vars) ->
          let (a',l) = tables.input.(Channel.to_int c) in
            l <> [] &&
            List.for_all
              (fun phi ->
                 List.exists
                   (fun (_,id,_) ->
                      let psi = Frame.of_id id in
                        Frame.prefix psi phi)
                   vars)
              (State.alive_frames s)

(** For any valid concretization, symbolic actions have disabled
  * instances iff they are symbolically disabled. *)
let may_be_disabled s a = not (may_be_enabled s a)

let indep_ee s a b =
  if Action.same_skel a b then false else
    let s_sa = steps s a in
    let s_sb = steps s b in
    let incompatible s1 s2 =
      not (Constraints.compatible s1.State.constraints s2.State.constraints)
    in
      assert (not (StateSet.is_empty s_sa)) ;
      assert (not (StateSet.is_empty s_sb)) ;
      StateSet.for_all
        (fun sa ->
           StateSet.for_all
             (fun sb ->
                incompatible sa sb ||
                let s_sab = steps sa b in
                let s_sba = steps sb a in
                  not (StateSet.is_empty s_sab) &&
                  not (StateSet.is_empty s_sba) &&
                  StateSet.for_all
                    (fun sab ->
                       incompatible sab sb ||
                       StateSet.for_all
                         (fun sba ->
                            incompatible sba sa ||
                            incompatible sba sab ||
                            State.equal sab sba)
                         s_sba)
                    s_sab)
             s_sb)
        s_sa

let indep_de s a b =
  let s_sb = steps s b in
    assert (not (StateSet.is_empty s_sb)) ;
    StateSet.for_all
      (fun sb ->
         not (may_be_enabled sb a))
      s_sb

let () =
  Check.add_suite
    ("Semantics",
     [ "Size of split_process", `Quick,
       (fun () ->
          let u = Term.var "u" in
          let v = Term.var "v" in
          let w = Term.var "w" in
          let c = Channel.of_int 0 in
          let d = Channel.of_int 1 in
          let p = Process.input c (Term.var "x") Process.zero in
          let q = Process.input d (Term.var "x") Process.zero in
          let r = Process.output c u Process.zero in
          let s = Process.output c v Process.zero in
          let cstr = Constraints.empty in
          let count p = List.length (SuccFail.list_of (split_process cstr p)) in
            Alcotest.(check int) "nb of splits for (if u=u then P else Q)"
              1
              (count (Process.if_eq u u p q)) ;
            Alcotest.(check int) "nb of splits for (if u=v then P else Q)"
              2
              (count (Process.if_eq u v p q)) ;
            Alcotest.(check int) "nb of splits for (if v=u then P else Q)"
              2
              (count (Process.if_eq v u p q)) ;
            Alcotest.(check int)
              "nb of splits for (if v=u then P else Q | if u=v then R else S)"
              2
              (count (Process.par [Process.if_eq v u p q;
                                   Process.if_eq u v r s])) ;
            Alcotest.(check int)
              "nb of splits for (if u=v then (if v=w then P else R) else Q)"
              3
              (count (Process.if_eq u v (Process.if_eq v w p r) q)) ;
            Alcotest.(check int)
              "nb of splits for (if v=u then P else Q | if u=w then R else S)"
              4
              (count (Process.par [Process.if_eq v u p q;
                                   Process.if_eq u w r s])) ;
            Alcotest.(check int)
              "nb of splits for (if v=u then P else 0 | if u=w then R else S)"
              4
              (count (Process.par [Process.if_eq v u p Process.zero;
                                   Process.if_eq u w r s])) ;
            Alcotest.(check int)
              "nb of splits for (if v=u then P else Q + if u=w then R else S)"
              4
              (count (Process.plus [Process.if_eq v u p q;
                                    Process.if_eq u w r s])) ;
            Alcotest.(check int)
              "nb of splits for \
               (if v=u then .. | if u=w then .. | if v=w then ..)"
              (* We do not perform congruence closure and miss than v=w
               * is a consequence of u=v and u=w. *)
              8
              (count (Process.par [Process.if_eq v u p q;
                                   Process.if_eq u w r s;
                                   Process.if_eq v w p p])) ;
       ) ;
       "Transitions", `Quick,
       (fun () ->
          let c = Channel.of_int 0 in
          let o = Process.output c (Term.ok ()) Process.zero in
          let io = Process.input c (Term.var "x") o in
          let s = { State.
                    left = Configs.of_process io ;
                    right = Configs.of_process o ;
                    constraints = Constraints.empty } in
          Format.printf "s = %a\n" State.pp s ;
          let tbl = transitions s in
          Alcotest.(check int)
            "no transition for channel 1"
            0
            (List.length (snd tbl.output.(1)) +
             List.length (snd tbl.input.(1))) ;
          Alcotest.(check int)
            "one input for channel 0"
            1
            (List.length (snd tbl.input.(0))) ;
          Alcotest.(check int)
            "one output for channel 0"
            1
            (List.length (snd tbl.output.(0))) ;
          Format.printf "s.out = %a\n"
            State.pp
            (List.hd (snd tbl.output.(0))) ;
          let s' = List.hd (snd tbl.input.(0)) in
          Format.printf "s.in = %a\n" State.pp s' ;
          Alcotest.(check bool)
            "correct value for s.in.out"
            true
            (State.equal s'
               { State.
                 left = Configs.of_process (Process.output c (Term.ok ()) Process.zero) ;
                 right = Configs.of_process (Process.bottom 0) ;
                 constraints = Constraints.empty }) ;
          let tbl' = transitions s' in
          Alcotest.(check int)
            "one in(0).out(0) trace"
            1
            (List.length (snd tbl'.output.(0))) ;
          let s'' = List.hd (snd tbl'.output.(0)) in
          Format.printf "s.in.out = %a\n" State.pp s'' ;
          Alcotest.(check bool)
            "correct value for s.in.out"
            true
            (State.equal s''
               { State.
                 left = Configs.add (Process.zero, Frame.append Frame.empty c (Term.ok ())) Configs.empty ;
                 right = Configs.of_process (Process.bottom 0) ;
                 constraints = Constraints.empty }) ;
          let tbl'' = transitions s'' in
          Alcotest.(check int)
            "nothing on 0 after in(0).out(0)"
            0
            (List.length (snd tbl''.input.(0)) +
             List.length (snd tbl''.output.(0))) ;
       ) ;
       "Trans. with non-det + cond (1)", `Quick,
       (fun () ->
          let c = Channel.of_int 0 in
          let o = Process.output c (Term.ok ()) Process.zero in
          let p = Process.if_eq (Term.var "x") (Term.ok ()) o Process.zero in
          let q = Process.input c (Term.var "x") p in
          let r = Process.input c (Term.var "x") Process.zero in
          let s = { State.
                    left = Configs.of_process (Process.par [q;r]) ;
                    right = Configs.empty ;
                    constraints = Constraints.empty } in
          let l = (snd (transitions s).input.(0)) in
            List.iter
              (fun s' -> Format.printf "%a -in(0)-> %a\n" State.pp s State.pp s')
              l ;
            Alcotest.(check int)
              "number of input transitions"
              2
              (List.length l)) ;
       "Trans. with non-det + cond (2)", `Quick,
       (fun () ->
          let c = Channel.of_int 0 in
          let o = Process.output c (Term.ok ()) Process.zero in
          let p = Process.if_eq (Term.var "x") (Term.ok ()) o Process.zero in
          let q = Process.input c (Term.var "x") p in
          let r = Process.input c (Term.var "x") Process.zero in
          let s = { State.
                    left = Configs.of_process (Process.par [q;r]) ;
                    right = Configs.of_process (Process.par [q;r]) ;
                    constraints = Constraints.empty } in
          let l = snd (transitions s).input.(0) in
            List.iter
              (fun s' -> Format.printf "%a -in(0)-> %a\n" State.pp s State.pp s')
              l ;
            Alcotest.(check int)
              "number of input transitions"
              2
              (List.length l)) ;
       "Cardinal of enabled and fold_successors", `Quick,
       (fun () ->
          let c = Channel.of_int 0 in
          let o = Process.output c (Term.ok ()) Process.zero in
          let io = Process.input c (Term.var "x") o in
          let s = { State.
                    left = Configs.of_process o ;
                    right = Configs.of_process io ;
                    constraints = Constraints.empty } in
            Alcotest.(check int)
              "cardinal of enabled"
              2
              (ActionSet.cardinal (enabled_cover s)) ;
            Alcotest.(check int)
              "cardinal of enabled"
              2
              (fold_successors s 0 (fun _ _ n -> n+1))) ;
       "Independence", `Quick,
       (fun () ->
          let c = Channel.of_int 0 in
          let o = Process.output c (Term.ok ()) Process.zero in
          let io = Process.input c (Term.var "x") o in
          let s = { State.
                    left = Configs.of_process (Process.par [io;o]) ;
                    right = Configs.empty ;
                    constraints = Constraints.empty } in
          let a_out = Action.Out (c,0) in
          let a_in = Action.In (c,[c,Frame.empty.Frame.id,0]) in
            Alcotest.(check bool)
              "input enabled"
              true
              (may_be_enabled s a_in) ;
            Alcotest.(check bool)
              "output enabled"
              true
              (may_be_enabled s a_out) ;
            Alcotest.(check bool)
              "parallel input and output are indep_ee"
              true
              (* the lack of action-determinism isn't a problem
               * since the two alternative outputs are identical *)
              (indep_ee s a_out a_in) ;
            let s' =
              let set = steps s a_out in
                Alcotest.(check int)
                  "nb of output alternatives"
                  1
                  (StateSet.cardinal set) ;
                StateSet.choose set
            in
            let phi = Frame.append Frame.empty c (Term.ok ()) in
            let a_in' = Action.In (c,[c,phi.Frame.id,0]) in
            Alcotest.(check bool)
              "input' enabled in s'"
              true
              (may_be_enabled s' a_in') ;
            Alcotest.(check bool)
              "input' not enabled in s"
              false
              (may_be_enabled s a_in') ;
            Alcotest.(check bool)
              "indep_de input' output in s"
              false
              (indep_de s a_in' a_out) ;
            Alcotest.(check bool)
              "indep_de output' output in s"
              true
              (indep_de s (Action.Out (c,1)) a_out) ;
       ) ;
     ] )
