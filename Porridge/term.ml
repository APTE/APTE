type 'a _term =
  | Fun of string * 'a list
  | Var of Channel.t * int * int (* channel, frame, freshness *)

type term = { id : int ; contents : term _term }

type invar = Channel.t * int * int

type t = term
let equal x y = x == y
let compare x y = compare x.id y.id
let hash x = x.id

(** HashedType instance for term representations whose subterms are
  * already hash-consed. *)
module PTerm : Hashtbl.HashedType with type t = term _term = struct

  type t = term _term

  let equal x y = match x,y with
    | Fun (f1,l1), Fun (f2,l2) ->
        f1 == f2 &&
        begin try
          List.iter2 (fun x y -> if x != y then raise Not_found) l1 l2 ; true
        with
          | Not_found | Invalid_argument _ -> false
        end
    | Var (c1,i1,n1), Var (c2,i2,n2) ->
        c1 = c2 && i1 = i2 && n1 = n2
    | _ -> false

  let hash t = match t with
    | Fun (s,l) -> Hashtbl.hash (s, List.map (fun x -> x.id) l)
    | Var (c,i,n) -> Hashtbl.hash ("", [Channel.to_int c;i;n])

end

module HTerm = Hashtbl.Make(PTerm)

let h = HTerm.create 257

let reset,new_id =
  let c = ref 0 in
    (fun () -> c := 0 ; HTerm.reset h),
    (fun () -> incr c ; !c)

let hashcons c =
  try HTerm.find h c with
    | Not_found ->
        let t = { id = new_id () ; contents = c } in
          HTerm.add h c t ;
          t

(** Physically unique string representations of function symbols *)
module Syms = struct
  let ok = "ok"
  let senc = "senc"
  let sdec = "sdec"
  let aenc = "aenc"
  let adec = "adec"
  let pk = "pk"
  let sign = "sign"
  let vk = "vk"
  let checksign = "checksign"
  let hash = "hash"
  let tuple = "tuple"
  let variables = Hashtbl.create 17
end

let invar c phi n = hashcons (Var (c,phi,n))
let ok () = hashcons (Fun (Syms.ok,[]))
let senc x y = hashcons (Fun (Syms.senc,[x;y]))
let sdec x y = hashcons (Fun (Syms.sdec,[x;y]))
let aenc x y = hashcons (Fun (Syms.aenc,[x;y]))
let adec x y = hashcons (Fun (Syms.adec,[x;y]))
let hash_tm x = hashcons (Fun (Syms.hash,[x]))
let pk x = hashcons (Fun (Syms.pk,[x]))
let sign x y = hashcons (Fun (Syms.sign,[x;y]))
let checksign x y = hashcons (Fun (Syms.checksign,[x;y]))
let vk x = hashcons (Fun (Syms.vk,[x]))
let hash x = hashcons (Fun (Syms.hash,[x]))
let tuple l = hashcons (Fun (Syms.tuple,l))
let var x =
  let x =
    try
      Hashtbl.find Syms.variables x
    with Not_found ->
      Hashtbl.add Syms.variables x x ;
      x
  in
    hashcons (Fun (x,[]))

let rec pp_list ch l =
  match l with
    | [] -> ()
    | [hd] ->
        pp ch hd
    | hd::tl ->
        pp ch hd ;
        Format.fprintf ch "," ;
        pp_list ch tl
and pp ch t =
  match t.contents with
    | Fun (sym,l) ->
        if sym = Syms.tuple then begin
          Format.fprintf ch "<%a>" pp_list l
        end else if l = [] then
          Format.fprintf ch "%s" sym
        else
          Format.fprintf ch "%s(%a)" sym pp_list l
    | Var (c,phi,i) ->
        Format.fprintf ch "X%c:%x:%x" (Channel.to_char c) phi i

let to_string t = Format.asprintf "%a" pp t

let rec subst t x y = match t.contents with
  | Fun (f,l) ->
     if t.id = x.id then y else
       hashcons (Fun (f, List.map (fun t -> subst t x y) l))
  | Var (c,phi,i) ->
      assert (t.id <> x.id) ; (* we only substitute invars for vars *)
      t

(** Basic tests on the unicity of term representations *)
let () =
  let tmtest : term Alcotest.testable = (module struct
    type t = term
    let pp = pp
    let equal = equal
  end : Alcotest.TESTABLE with type t = term) in
  Check.add_suite
    ("Term",
     [ "Initial size", `Quick,
       (fun () ->
          reset () ;
          Alcotest.(check int) "hashtbl length" 0 (HTerm.length h)) ;
       "Simple size", `Quick,
       (fun () ->
          reset () ;
          let ok = ok () in
          ignore (senc (tuple [hash ok; hash (hash ok)]) ok) ;
          Alcotest.(check int) "hashtbl length" 5 (HTerm.length h)) ;
       "Simple size", `Quick,
       (fun () ->
          reset () ;
          let ok = ok () in
          ignore (senc (tuple [hash ok; hash ok]) ok) ;
          Alcotest.(check int) "hashtbl length" 4 (HTerm.length h)) ;
       "Structural inequality senc/aenc", `Quick,
       (fun () ->
          reset () ;
          let t1 = aenc (ok ()) (ok ()) in
          let t2 = senc (ok ()) (ok ()) in
            match (tuple [t1;t2]).contents with
              | Fun (s,[a;b]) when s = "tuple" ->
                  Alcotest.(check bool) "structural equality" false (a = b)
              | _ -> assert false) ;
       "Physical equality", `Quick,
       (fun () ->
          reset () ;
          let t = aenc (ok ()) (hash (ok ())) in
          let t = senc t t in
            match t.contents with
              | Fun (s,[a;b]) when s == Syms.senc ->
                  Alcotest.(check bool) "physical equality" true (a==b)
              | _ -> assert false) ;
       "Physical eq. requirement on symbols", `Quick,
       (fun () ->
          reset () ;
          Alcotest.(check bool)
            "physical equality"
            false
            (hashcons (Fun ("ha"^"sh",[ok ()])) ==
             hash (ok ()))) ;
       "Serialization", `Quick,
       (fun () ->
          reset () ;
          Alcotest.(check string)
            "string representation"
            "senc(hash(ok),<ok,ok>)"
            (to_string (senc (hash (ok ())) (tuple [ok ();ok ()])))) ;
       "Substitution", `Quick,
       (fun () ->
          Alcotest.(check tmtest)
            "correct substitution result"
            (hash (invar Channel.c 1 2))
            (subst (hash (var "x")) (var "x") (invar Channel.c 1 2))) ;
     ])

let () =
  Check.add_suite
    ("PTerm",
     [ "Basic equalities test", `Quick,
       (fun () ->
          reset () ;
          let t1 = ok () in
          let t2 = ok () in
          let a = Fun (Syms.aenc,[t1;t2]) in
          let b = Fun (Syms.aenc,[t1;t2]) in
            Alcotest.(check bool) "physically different" (a != b) true ;
            Alcotest.(check bool) "structurally equal" (a = b) true ;
            Alcotest.(check bool) "PTerm.equal" (PTerm.equal a b) true) ])

(* TODO random testing e.g.
 * for all [x : term _term], [x = (hashcons x).contents]. *)
