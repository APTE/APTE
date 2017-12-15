(** This module defines several datatypes to be used in [Semantics]. *)

open Frame

(** Generic constraint representation *)
module Constraints : sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val empty : t
  val pp : Format.formatter -> t -> unit
  val add_f : t -> Formula.t -> t option
  val add_f_neg : t -> Formula.t -> t option
  val compatible : t -> t -> bool
end = struct

  (* The list is ordered and equalities are ordered too. *)
  type t = (Term.term * Term.term * bool) list

  let hash t =
    Hashtbl.hash (List.map (fun (s,t,b) -> Term.hash s, Term.hash t, b) t)

  let rec compare c1 c2 =
    let rec aux = function
      | (s1,t1,b1)::tl1, (s2,t2,b2)::tl2 ->
	 let c = Term.compare s1 s2 in
	 if c <> 0 then c else
           let c = Term.compare t1 t2 in
	   if c <> 0 then c else
	     let c = Pervasives.compare b1 b2 in
	     if c <> 0 then c else
	       aux (tl1, tl2)
      | [],[] -> 0
      | [],_ -> -1
      | _,[] -> 1 in
    aux (c1,c2)

  let equal t1 t2 = compare t1 t2 = 0

  let empty = []

  exception Conflict

  let rec add_cstr_e c s t b =
    match c with
      | [] -> [s,t,b]
      | (s',t',b')::tl ->
          match Term.compare s s', Term.compare t t' with
            | 0,0 ->
                if b = b' then c else raise Conflict
            | 1,_ | 0,1 ->
                (s,t,b) :: c
            | _ ->
                (s',t',b') :: add_cstr_e tl s t b

  let add_cstr_e c s t b =
    match Term.compare s t with
      | 0 -> if b then Some c else None
      | 1 -> (try Some (add_cstr_e c s t b) with Conflict -> None)
      | _ -> (try Some (add_cstr_e c t s b) with Conflict -> None)

  let rec add_f c (f:Formula.t) =
    match f.Formula.contents with
    | Formula.Eq (t1,t2) ->
       (match Term.compare t1 t2 with
	| 0 -> Some c
	| 1 -> (try add_cstr_e c t1 t2 true with Conflict -> None)
	| _ -> (try add_cstr_e c t2 t1 true with Conflict -> None))
    | Formula.Neq (t1,t2) ->
       (match Term.compare t1 t2 with
	| 0 -> None
	| 1 -> (try add_cstr_e c t1 t2 false with Conflict -> None)
	| _ -> (try add_cstr_e c t2 t1 false with Conflict -> None))
    | Formula.And (f1,f2) ->
       (match add_f c f1 with
	| Some c' -> add_f c' f2
	| None -> None)
    | Formula.Or (f1,f2) -> Printf.printf "Porridge does not handle formulae with Or yet.\n%!"; exit 1   (* LH: I've tried to deal with OR by representing constraints as sets of OR-clauses but it seems too costly. So instead, I "flatten" processes with and or or in conditionals before calling porridge. *)

  (* Add negation of a formula *)
  let rec add_f_neg c (f:Formula.t) =
    match f.Formula.contents with
    | Formula.Eq (t1,t2) ->
       (match Term.compare t1 t2 with
	| 0 -> None
	| 1 -> (try add_cstr_e c t1 t2 false with Conflict -> None)
	| _ -> (try add_cstr_e c t2 t1 false with Conflict -> None))
    | Formula.Neq (t1,t2) ->
       (match Term.compare t1 t2 with
	| 0 -> Some c
	| 1 -> (try add_cstr_e c t1 t2 true with Conflict -> None)
	| _ -> (try add_cstr_e c t2 t1 true with Conflict -> None))
    | Formula.Or (f1,f2) ->
       (match add_f_neg c f1 with
	| Some c' -> add_f_neg c' f2
	| None -> None)
    | Formula.And (f1,f2) -> Printf.printf "Porridge does not handle negation of formulae with And yet.\n%!"; exit 0

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

  let pp ch =
    List.iter
      (fun (s,t,b) ->
         Format.fprintf ch "_%a%s%a"
           Term.pp s
           (if b then "=" else "≠")
           Term.pp t)

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
  let hash (p,phi) = Hashtbl.hash (Process.hash p,Frame.hash phi)
  let equal (p,phi) (q,psi) = Process.equal p q && Frame.equal phi psi
  let compare (p,phi) (q,psi) =
    let c = Process.compare p q in
      if c = 0 then Frame.compare phi psi else c
  let pp ch (p,phi) =
    Format.fprintf ch "@[<1><%a%a>@]" Process.pp p Frame.pp phi
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
    | hd::tl -> Format.fprintf ch "%a+@,%a" Config.pp hd pp tl

  let pp ch t =
   Format.fprintf ch "@[" ;
   pp ch t.contents ;
   Format.fprintf ch "@]"

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

  let singleton c = mk_configs [c]

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

  let length c = List.length c.contents

end

let () =
  Check.add_suite
    ("Configs",
     [ "List round-trip", `Quick,
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
            Alcotest.check (module Configs) "configs are equal"
              s1 s2 ;
            Alcotest.(check int)
              "config is of size 2"
              2
              (Configs.length s) ;
            List.iter
              (fun (c:Configs.t) ->
                 Alcotest.(check bool) "configs are equal" true
                   (Configs.equal c (Configs.of_list (Configs.to_list c))))
              [s;s1;s2]) ;
     ])

let () =
  Check.add_suite
    ("Constraints",
     [ "Ordering", `Quick,
       begin fun () ->
          let x = Term.var "x" in
          let y = Term.var "y" in
          let z = Term.var "z" in
          let conj f f' =
            match Constraints.add_f Constraints.empty f' with
              | None -> assert false
              | Some x ->
                  match Constraints.add_f x f with
                    | None -> assert false
                    | Some y -> y
          in
          let c = conj (Formula.form_eq x y) (Formula.form_neq y z) in
          let c' = conj (Formula.form_neq y z) (Formula.form_eq x y) in
          let d =
            match
              Constraints.add_f Constraints.empty (Formula.form_eq z y)
            with
              | None -> assert false
              | Some x -> x
          in
            Alcotest.check (module Constraints)
              "x=y∧y≠z = y≠z∧x=y"
              c c' ;
            Alcotest.check (module Constraints)
              "(z=y)∧(z=y) = (z=y)"
              (conj (Formula.form_eq z y) (Formula.form_eq z y)) d ;
            Alcotest.check (module Constraints)
              "(z=y)∧(y=z) = (z=y)"
              (conj (Formula.form_eq z y) (Formula.form_eq y z)) d ;
            Alcotest.(check bool)
              "x=y∧y≠z incompatible with z=y"
              false
              (Constraints.compatible c d) ;
            Alcotest.(check bool)
              "y≠z∧x=y incompatible with z=y"
              false
              (Constraints.compatible c' d)
       end
     ])
