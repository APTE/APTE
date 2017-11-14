(** This module defines several datatypes to be used in [Semantics]. *)

(** Lists of terms, hash-consed. *)
module Term_List : sig
  type 'l lst = Nil | Cons of Term_.term * 'l
  type t = private { id : int ; contents : t lst }
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val nil : t
  val is_empty : t -> bool
  val cons : (Term_.term*t) -> t
  val prefix : t -> t -> bool
  val length : t -> int
  val pp : Format.formatter -> t -> unit
end = struct

  type 'l lst = Nil | Cons of Term_.term * 'l

  type t = { id : int ; contents : t lst }

  let equal t1 t2 = t1.id = t2.id
  let hash t = t.id
  let compare t1 t2 = Pervasives.compare t1.id t2.id

  let nil = { id = 0 ; contents = Nil }

  let is_empty t = t.contents = Nil

  (** HashedType instance for non-empty lists whose sublist is hash-consed. *)
  module PT = struct
    type _t = Term_.term * t
    type t = _t
    let equal (t,l) (t',l') =
      Term_.equal t t' && l.id = l'.id
    let hash (t,l) =
      Hashtbl.hash (Term_.hash t,l.id)
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
    | Cons (t,{contents=Nil}) -> Format.fprintf ch "%a" Term_.pp t
    | Cons (t,l) -> Format.fprintf ch "%a,%a" Term_.pp t pp l

  let rec length t = match t.contents with
    | Nil -> 0
    | Cons (_,t) -> 1 + length t

end

(** Frames, hash-consed. *)
module Frame : sig
  type t = private { id : int ; contents : Term_List.t array }
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val empty : t
  val append : t -> Channel.t -> Term_.term -> t
  val prefix : t -> t -> bool
  val size : t -> int
  val size_on_channel : t -> Channel.t -> int
  val of_id : int -> t
end = struct

  (** We use a handle w_{c,i} for the i-th output on channel c.
    * This is implicit in the representation: a frame is an array
    * with one cell per channel, containing the list of outputs on
    * that channel, with the latest output at the head.
    * Frames have a unique identifier. *)
  type t = { id : int ; contents : Term_List.t array }

  let equal t1 t2 = t1.id = t2.id
  let hash t = t.id
  let compare t1 t2 = Pervasives.compare t1.id t2.id

  module PF = struct
    type t = Term_List.t array
    let hash f =
      (* TODO avoid Array.map *)
      Hashtbl.hash (Array.map (fun x -> x.Term_List.id) f)
    let equal f g =
      try
        Array.iter2
          (fun x y -> if not (Term_List.equal x y) then raise Exit)
          f g ;
        true
      with
        | Exit -> false
  end
  module H = Hashtbl.Make(PF)

  (** Table of already created frames: associates to each content array
    * a unique representant of type t. *)
  let h : t H.t = H.create 257
  let id_table = Hashtbl.create 257

  let next_id = ref 0

  let mk_frame phi =
    try H.find h phi with
      | Not_found ->
          let frame = { id = !next_id ; contents = phi } in
          H.add h phi frame ;
          Hashtbl.add id_table frame.id frame ;
          incr next_id ;
          frame

  let of_id id = Hashtbl.find id_table id

  (** The empty frame. *)
  let empty = mk_frame (Array.make Channel.nb_chan Term_List.nil)

  (** Return a new frame containing one more term for the given channel. *)
  let append frame channel term =
    let channel = Channel.to_int channel in
      mk_frame
        (Array.mapi
           (fun i l -> if i = channel then Term_List.cons (term,l) else l)
           frame.contents)

  let prefix phi psi =
    let phi = phi.contents in
    let psi = psi.contents in
    let rec aux i =
      if i = Channel.nb_chan then true else
        Term_List.prefix phi.(i) psi.(i) && aux (i+1)
    in aux 0

  let pp ch phi =
    let phi = phi.contents in
    for i = 0 to Channel.nb_chan - 1 do
      if not (Term_List.is_empty phi.(i)) then
        Format.fprintf ch ";%d=%a" i Term_List.pp phi.(i)
    done

  let size phi =
    Array.fold_left (fun n l -> n + Term_List.length l) 0 phi.contents

  let size_on_channel phi c =
    Term_List.length phi.contents.(Channel.to_int c)

end

(** Equality constraint representation *)
module Constraints : sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val empty : t
  val pp : Format.formatter -> t -> unit
  val add_eq : t -> Term_.term -> Term_.term -> t option
  val add_neq : t -> Term_.term -> Term_.term -> t option
  val compatible : t -> t -> bool
end = struct

  (* The list is ordered and equalities are ordered too. *)
  type t = (Term_.term * Term_.term * bool) list

  let hash t = Hashtbl.hash (List.map (fun (s,t,b) -> s.Term_.id, t.Term_.id, b) t)
  let rec compare t1 t2 =
    match t1,t2 with
      | (s1,t1,b1)::l1, (s2,t2,b2)::l2 ->
          let c = Term_.compare s1 s2 in
            if c <> 0 then c else
              let c = Term_.compare t1 t2 in
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
      | [] -> [s,t,b]
      | (s',t',b')::tl ->
          match Term_.compare s s', Term_.compare t t' with
            | 0,0 ->
                if b = b' then c else raise Conflict
            | 1,_ | 0,1 ->
                (s,t,b) :: c
            | _ ->
                (s',t',b') :: add_cstr tl s t b

  let add_cstr c s t b =
    match Term_.compare s t with
      | 0 -> if b then Some c else None
      | 1 -> (try Some (add_cstr c s t b) with Conflict -> None)
      | _ -> (try Some (add_cstr c t s b) with Conflict -> None)

  let add_eq c s t = add_cstr c s t true
  let add_neq c s t = add_cstr c s t false

  let rec compatible c1 c2 =
    match c1,c2 with
      | [],_ | _,[] -> true
      | (s1,t1,b1)::c1', (s2,t2,b2)::c2' ->
          match Term_.compare s1 s2, Term_.compare t1 t2 with
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
           Term_.pp s
           (if b then "=" else "≠")
           Term_.pp t)
      t

end

(** A configuration is a pair of a process and a frame,
  * straightforwardly implemented as such. *)
module Config : sig
  type t = Process_.t * Frame.t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Process_.t * Frame.t
  let hash (p,phi) = Hashtbl.hash (Process_.hash p,phi.Frame.id)
  let equal (p,phi) (q,psi) = Process_.equal p q && Frame.equal phi psi
  let compare (p,phi) (q,psi) =
    let c = Process_.compare p q in
      if c = 0 then Frame.compare phi psi else c
  let pp ch (p,phi) =
    Format.fprintf ch "<%a%a>" Process_.pp p Frame.pp phi
end

(** Sets of configurations. *)
module Configs = struct

  (* We need an ordered hashed type, so we might as well hash-cons it too.
   * The strategy is similar to what is done in Frame but with ordered lists
   * of configurations. *)

  type contents = Config.t list
  type t = { id : int ; contents : contents }

  let equal t1 t2 = t1.id = t2.id
  let hash t = t.id
  let compare t1 t2 = Pervasives.compare t1.id t2.id

  let rec pp ch = function
    | [] -> Format.fprintf ch "ø"
    | [e] -> Config.pp ch e
    | hd::tl -> Format.fprintf ch "%a+%a" Config.pp hd pp tl

  let pp ch t = pp ch t.contents

  module PConfigs = struct
    type t = contents
    let equal l1 l2 =
      try
        List.for_all2 Config.equal l1 l2
      with
        | Invalid_argument _ -> false
    let hash l = Hashtbl.hash (List.map Config.hash l)
  end

  module H = Hashtbl.Make(PConfigs)
  let h = H.create 257

  let next_id = ref 0

  let mk_configs s =
    try H.find h s with
      | Not_found ->
          let c = { id = !next_id ; contents = s } in
            incr next_id ;
            H.add h s c ;
            c

  let empty = mk_configs []

  let rec insert c = function
    | [] -> [c]
    | hd::tl ->
        match Config.compare hd c with
          | 0 -> hd::tl
          | 1 -> c::hd::tl
          | _ -> hd::(insert c tl)

  let add c s = mk_configs (insert c s.contents)

  let to_list c = c.contents

  let of_list l = mk_configs (List.sort Config.compare l)

  let of_process p = mk_configs [p,Frame.empty]

end

let () =
  Check.add_suite
    ("Configs",
     [ "List round-trip", `Quick,
       (fun () ->
          let c0 = Process_.bottom 0,Frame.empty in
          let c1 = Process_.bottom 1,Frame.empty in
          let c2 = Process_.bottom 1,Frame.empty in
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
