(** We define terms, list of terms, and frames, mutually recursively *)

module Domain : sig
  type t = int Channel.Map.t
  val hash : t -> int
  val equal : t -> t -> bool
  val included : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val empty : t
  val size_on_channel : t -> Channel.t -> int
  val size : t -> int
  val add : t -> Channel.t -> t
  val union : t -> t -> t
  val of_list : Channel.t list -> t
end = struct
  type t = int Channel.Map.t
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare
  let pp ch d =
    Channel.Map.iter
      (fun c n ->
         assert (n > 0) ;
         Format.fprintf ch ",@,%c^%d" (Channel.to_char c) n)
      d
  let empty = Channel.Map.empty
  let size_on_channel dom c = Channel.Map.get dom c
  let size dom = Channel.Map.fold (fun size _ n -> size+n) 0 dom
  let add dom c = Channel.Map.update_or_insert c (fun n -> n+1) 1 dom
  let of_list l = List.fold_left add empty l
  let union d d' =
    Channel.Map.merge
      (fun c -> function
         | `Left x | `Right x -> x
         | `Both (x,y) -> if x <= y then y else x)
      d d'
  let included d d' = (union d d') = d' (* TODO simplify *)
end

module rec Term : Tm.S with type invar = Invar.t = Tm.Make(Invar)

and Invar : Tm.HashedType with type t = Channel.t*int*Frame.t = struct
  type t = Channel.t * int * Frame.t
  let equal (c,i,phi) (d,j,psi) =
    c = d &&
    i = j &&
    Frame.equal phi psi
  let hash v = Hashtbl.hash v
  let pp ch (c,i,phi) =
    Format.fprintf ch "X^%c,%d_%a"
      (Channel.to_char c) i
      Frame.pp_domain phi
end

and TermList : sig
  type 'l lst = Nil | Cons of Term.term * 'l
  type t = private { id : int ; contents : t lst }
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val nil : t
  val is_empty : t -> bool
  val cons : Term.term -> t -> t
  val prefix : t -> t -> bool
  val length : t -> int
  val pp : Format.formatter -> t -> unit
end = struct

  type 'l lst = Nil | Cons of Term.term * 'l

  type t = { id : int ; contents : t lst }

  let equal t1 t2 = t1.id = t2.id
  let hash t = t.id
  let compare t1 t2 = Pervasives.compare t1.id t2.id

  let nil = { id = 0 ; contents = Nil }

  let is_empty t = t.contents = Nil

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
  let cons x y = cons (x,y)

  let rec prefix l1 l2 =
    if l1 == l2 then true else
      match l2.contents with
        | Nil -> false
        | Cons (_,l3) -> prefix l1 l3

  let rec pp ch t = match t.contents with
    | Nil -> Format.fprintf ch "ø"
    | Cons (t,{contents=Nil}) -> Format.fprintf ch "%a" Term.pp t
    | Cons (t,l) -> Format.fprintf ch "%a,%a" Term.pp t pp l

  let rec length t = match t.contents with
    | Nil -> 0
    | Cons (_,t) -> 1 + length t

end

(** Frames, hash-consed. *)
and Frame : sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val pp_domain : Format.formatter -> t -> unit
  val empty : t
  val append : t -> Channel.t -> Term.term -> t
  val prefix : t -> t -> bool
  val size : t -> int
  val size_on_channel : t -> Channel.t -> int
  val restrict : t -> Domain.t -> t
  val domain : t -> Domain.t
end = struct

  (** We use a handle w_{c,i} for the i-th output on channel c.
    * This is implicit in the representation: a frame is an array
    * with one cell per channel, containing the list of outputs on
    * that channel, with the latest output at the head.
    * Frames have a unique identifier. *)
  type t = { id : int ; contents : TermList.t Channel.Map.t }

  let equal t1 t2 = t1.id = t2.id
  let hash t = t.id
  let compare t1 t2 = Pervasives.compare t1.id t2.id

  module PF = struct
    type t = TermList.t Channel.Map.t
    let hash phi =
      (* TODO avoid Array.map *)
      Hashtbl.hash (Channel.Map.map (fun _ l -> TermList.hash l) phi)
    let equal f g =
      Channel.Map.for_all2
        (fun c x y -> TermList.equal x y)
        f g
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
  let empty = mk_frame Channel.Map.empty

  (** Return a new frame containing one more term for the given channel. *)
  let append frame channel term =
    mk_frame
      (Channel.Map.update_or_insert
         channel
         (fun l -> TermList.cons term l)
         (TermList.cons term TermList.nil)
         frame.contents)

  let prefix phi psi =
    Channel.Map.for_all2
      (fun c l1 l2 -> TermList.prefix l1 l2)
      phi.contents psi.contents

  let pp ch phi =
    Channel.Map.iter
      (fun c l ->
         Format.fprintf ch ";%c=%a"
           (Channel.to_char c)
           TermList.pp l)
      phi.contents

  let pp_domain ch phi =
    let first = ref true in
      Format.fprintf ch "{" ;
      Channel.Map.iter
        (fun c l ->
           if !first then
             first := false
           else
             Format.fprintf ch "," ;
           Format.fprintf ch "%c^%d"
             (Channel.to_char c)
             (TermList.length l))
        phi.contents ;
      Format.fprintf ch "}"

  let size phi =
    Channel.Map.fold (fun n _ l -> n + TermList.length l) 0 phi.contents

  let size_on_channel phi c =
    try TermList.length (Channel.Map.get phi.contents c) with Not_found -> 0

  let rec remove_last_n l n =
    if n <= 0 then l else
      match l.TermList.contents with
        | TermList.Nil -> assert false
        | TermList.Cons (hd,tl) -> remove_last_n tl (n-1)

  let restrict phi dom =
    mk_frame
      (Channel.Map.merge_intersect
         (fun c l n ->
            let ln = TermList.length l in
              remove_last_n l (ln-n))
         phi.contents dom)

  let domain phi = Channel.Map.map (fun c l -> TermList.length l) phi.contents

end
