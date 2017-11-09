(** This module defines several datatypes to be used in [Semantics]. *)

external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"

let iter2 f a b =
  if Array.length a <> Array.length b then
    invalid_arg "Array.iter2: arrays must have the same length"
  else
    for i = 0 to Array.length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

(** Lists of terms, hash-consed. *)
module TermList : sig
  type 'l lst = Nil | Cons of Term.term * 'l
  type t = private { id : int ; contents : t lst }
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val nil : t
  val cons : (Term.term*t) -> t
  val prefix : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end = struct

  type 'l lst = Nil | Cons of Term.term * 'l

  type t = { id : int ; contents : t lst }

  let equal t1 t2 = t1.id = t2.id
  let hash t = Hashtbl.hash t.id
  let compare t1 t2 = Pervasives.compare t1.id t2.id

  let nil = { id = 0 ; contents = Nil }

  (** HashedType instance for non-empty lists whose sublist is hash-consed. *)
  module PT = struct
    type _t = Term.term * t
    type t = _t
    let equal (t,l) (t',l') =
      Term.equal t t' && l.id = l'.id
    let hash (t,l) =
      Hashtbl.hash (Term.hash t,l.id)
  end

  module M = Memo.Make(PT)
  let count = ref 0
  let cons = M.make (fun (t,l) ->
                       incr count ;
                       { id = !count ;
                         contents = Cons (t,l) })

  let rec prefix l1 l2 =
    if l1 == l2 then true else
      match l2.contents with
        | Nil -> false
        | Cons (_,l3) -> prefix l1 l3

  let rec pp ch t = match t.contents with
    | Nil -> Format.fprintf ch "ø"
    | Cons (t,l) -> Format.fprintf ch "%a,%a" Term.pp t pp l

end

(** Frames, hash-consed. *)
module Frame : sig
  type t = private { id : int ; contents : TermList.t array }
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val empty : t
  val append : t -> Channel.t -> Term.term -> t
  val prefix : t -> t -> bool
end = struct

  (** We use a handle w_{c,i} for the i-th output on channel c.
    * This is implicit in the representation: a frame is an array
    * with one cell per channel, containing the list of outputs on
    * that channel, with the latest output at the head.
    * Frames have a unique identifier. *)
  type t = { id : int ; contents : TermList.t array }

  let equal t1 t2 = t1.id = t2.id
  let hash t = Hashtbl.hash t.id
  let compare t1 t2 = Pervasives.compare t1.id t2.id

  module PF = struct
    type t = TermList.t array
    let hash f =
      (* TODO avoid Array.map *)
      Hashtbl.hash (Array.map (fun x -> x.TermList.id) f)
    let equal f g =
      try
        iter2
          (fun x y -> if not (TermList.equal x y) then raise Exit)
          f g ;
        true
      with
        | Exit -> false
  end
  module H = Hashtbl.Make(PF)

  (** Table of already created frames: associates to each content array
    * a unique representant of type t. *)
  let h : t H.t = H.create 257

  let next_id = ref 0

  let mk_frame phi =
    try H.find h phi with
      | Not_found ->
          let frame = { id = !next_id ; contents = phi } in
          H.add h phi frame ;
          incr next_id ;
          frame

  (** The empty frame. *)
  let empty = mk_frame (Array.make Channel.nb_chan TermList.nil)

  (** Return a new frame containing one more term for the given channel. *)
  let append frame channel term =
    let channel = Channel.to_int channel in
      mk_frame
        (Array.mapi
           (fun i l -> if i = channel then TermList.cons (term,l) else l)
           frame.contents)

  let prefix phi psi =
    let phi = phi.contents in
    let psi = psi.contents in
    let rec aux i =
      if i = Channel.nb_chan then true else
        TermList.prefix phi.(i) psi.(i) && aux (i+1)
    in aux 0

  let pp ch phi =
    let phi = phi.contents in
    for i = 0 to Channel.nb_chan - 1 do
      Format.fprintf ch ";%d=%a" i TermList.pp phi.(i)
    done

end

(** Equality constraint representation *)
module Constraints : sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val empty : t
  val pp : Format.formatter -> t -> unit
  val add_eq : t -> Term.term -> Term.term -> t option
  val add_neq : t -> Term.term -> Term.term -> t option
  val compatible : t -> t -> bool
end = struct

  (* The list is ordered and equalities are ordered too. *)
  type t = (Term.term * Term.term * bool) list

  let hash t = Hashtbl.hash (List.map (fun (s,t,b) -> s.Term.id, t.Term.id, b) t)
  let rec compare t1 t2 =
    match t1,t2 with
      | (s1,t1,b1)::l1, (s2,t2,b2)::l2 ->
          let c = Term.compare s1 s2 in
            if c <> 0 then c else
              let c = Term.compare t1 t2 in
                if c <> 0 then c else
                  let c = Pervasives.compare b1 b2 in
                    if c <> 0 then c else
                      compare l1 l2
      | [],[] -> 0
      | [],_ -> -1
      | _,[] -> 1
  let equal t1 t2 = compare t1 t2 = 0

  let empty = []

  exception Conflict

  let rec add_cstr c s t b =
    match c with
      | [] -> [s,t,true]
      | (s',t',b')::tl ->
          match Term.compare s s', Term.compare t t' with
            | 0,0 ->
                if b = b' then c else raise Conflict
            | 1,_ | 0,1 ->
                (s,t,b) :: c
            | _ ->
                (s',t',b') :: add_cstr tl s t b

  let add_cstr c s t b =
    match Term.compare s t with
      | 0 -> if b then Some c else None
      | 1 -> (try Some (add_cstr c s t b) with Conflict -> None)
      | _ -> (try Some (add_cstr c t s b) with Conflict -> None)

  let add_eq c s t = add_cstr c s t true
  let add_neq c s t = add_cstr c s t false

  let rec compatible c1 c2 =
    match c1,c2 with
      | [],_ | _,[] -> true
      | (s1,t1,b1)::c1', (s2,t2,b2)::c2' ->
          match Term.compare s1 s2, Term.compare t1 t2 with
            | 0,0 ->
                b1 = b2 && compatible c1' c2'
            | 1,_ | 0,1 ->
                compatible c1' c2
            | _ ->
                compatible c1 c2'

  let pp ch t =
    List.iter
      (fun (s,t,b) ->
         Format.fprintf ch "/%a%s%a"
           Term.pp s
           (if b then "=" else "!=")
           Term.pp t)
      t

end

(** Processes with constraints and elementary semantics. *)
module SymProc = struct

  type t = {
    process : Process.t ;
    constraints : Constraints.t
  }

  let hash t =
    Hashtbl.hash (t.process.Process.id, Constraints.hash t.constraints)

  let compare t1 t2 =
    let c = Process.compare t1.process t2.process in
      if c = 0 then Constraints.compare t1.constraints t2.constraints else c

  let equal t1 t2 = compare t1 t2 = 0

  let pp ch t =
    Process.pp ch t.process ;
    Constraints.pp ch t.constraints

  (** Preliminary representation of all possible transition outcomes.
    * Substitutions are suspended, represented as a function.
    * Duplicates will be removed in the next stages, in Semantics. *)
  type trans_table = {
    output : (Term.term*t) list array ;
    input : (Term.term->t) list array
  }

  (* TODO memoize ? *)
  let transitions t =
    let output : (Term.term*t) list array = Array.make Channel.nb_chan [] in
    let input = Array.make Channel.nb_chan [] in
    let add_input c f = input.(Channel.to_int c) <- f::input.(Channel.to_int c) in
    let add_output c t p = output.(Channel.to_int c) <- (t,p)::output.(Channel.to_int c) in
    let rec aux context constraints proc = match proc.Process.contents with
      | Process.Zero -> ()
      | Process.Input (c,x,p) ->
          add_input c
            (fun y ->
               { process =
                   Process.par (Process.subst p x y :: context) ;
                 constraints })
      | Process.Output (c,t,p) ->
          add_output c t { process = p ; constraints }
      | Process.Plus l ->
          List.iter (aux context constraints) l
      | Process.Par l ->
          let rec try_all context = function
            | [] -> ()
            | p::l ->
                aux (List.rev_append l context) constraints p ;
                try_all (p::context) l
          in try_all context l
      | Process.If (a,b,t,e) ->
          begin match Constraints.add_eq constraints a b with
            | Some c' -> aux context c' t
            | None -> ()
          end ;
          begin match Constraints.add_neq constraints a b with
            | Some c' -> aux context c' e
            | None -> ()
          end
      | Process.Bottom i -> ()
    in
      aux [] t.constraints t.process ;
      { output ; input }

end

(** A configuration is a pair of a process and a frame,
  * straightforwardly implemented as such. *)
module Config : sig
  type t = Process.t * Frame.t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Process.t * Frame.t
  let hash (p,phi) = Hashtbl.hash (Process.hash p,phi.Frame.id)
  let equal (p,phi) (q,psi) = Process.equal p q && Frame.equal phi psi
  let compare (p,phi) (q,psi) =
    let c = Process.compare p q in
      if c = 0 then Frame.compare phi psi else c
  let pp ch (p,phi) =
    Format.fprintf ch "<%a%a>" Process.pp p Frame.pp phi
end

(** Sets of configurations. *)
module Configs = struct

  (* We need an ordered hashed type, so we might as well hash-cons it too.
   * The strategy is similar to what is done in Frame but with ordered lists
   * of configurations. *)

  type contents = Config.t list
  type t = { id : int ; contents : contents }

  let equal t1 t2 = t1.id = t2.id
  let hash t = Hashtbl.hash t.id
  let compare t1 t2 = Pervasives.compare t1.id t2.id

  let rec pp ch = function
    | [] -> ()
    | [e] -> Config.pp ch e
    | hd::tl -> Format.fprintf ch "%a+%a" Config.pp hd pp tl

  let pp ch t = pp ch t.contents

  module PConfigs = struct
    type t = contents
    let equal l1 l2 =
      List.for_all2 Config.equal l1 l2
    let hash l = Hashtbl.hash (List.map Config.hash l)
  end

  module H = Hashtbl.Make(PConfigs)
  let h = H.create 257

  let next_id = ref 0

  let mk_configs s =
    try H.find h s with
      | Not_found ->
          let c = { id = !next_id ; contents = s } in
            H.add h s c ;
            c

  let empty = mk_configs []

  let rec insert c = function
    | [] -> [c]
    | hd::tl ->
        match Config.compare hd c with
          | 0 -> hd::tl
          | -1 -> c::hd::tl
          | _ -> hd::(insert c tl)

  let add c s = mk_configs (insert c s.contents)

  let to_list c = c.contents

  let of_list l = mk_configs (List.sort Config.compare l)

end

let () =
  Check.add_suite
    ("Configs",
     [ "list round-trip", `Quick,
       (fun () ->
          let c0 = Process.bottom 0,Frame.empty in
          let c1 = Process.bottom 1,Frame.empty in
          let c2 = Process.bottom 1,Frame.empty in
          let s = Configs.add c0 (Configs.add c1 (Configs.add c2 Configs.empty)) in
          let s1 = Configs.add c1 (Configs.add c0 (Configs.add c2 Configs.empty)) in
          let s2 = Configs.add c2 (Configs.add c1 (Configs.add c0 Configs.empty)) in
            Alcotest.(check bool) "configs are equal"
              true
              (Configs.equal s s1) ;
            Alcotest.(check bool) "configs are equal"
              true
              (Configs.equal s s2) ;
            Alcotest.(check bool) "configs are equal"
              true
              (Configs.equal s1 s2) ;
            List.iter
              (fun (c:Configs.t) ->
                 Alcotest.(check bool) "configs are equal" true
                   (Configs.equal c (Configs.of_list (Configs.to_list c))))
              [s;s1;s2]) ;
     ])
