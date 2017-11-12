open Channel

type term = Term_.term

type ('a,'t) _proc =
  | Zero
  | Par of 'a list
  | Plus of 'a list
  | If of 't * 't * 'a * 'a
  | Input of channel * 't * 'a
  | Output of channel * 't * 'a
  | Bottom of int

type proc = { id : int ; contents : (proc,term) _proc }

type t = proc
let equal x y = x == y
let compare x y = compare x.id y.id
let hash x = x.id

(** HashedType instance for processes whose subprocesses are hash-consed. *)
module PProc : Hashtbl.HashedType with type t = (proc,term) _proc = struct

  type t = (proc,term) _proc

  let equal x y = match x,y with
    | Zero, Zero -> true
    | Par l, Par l' ->
        begin try
          List.iter2 (fun x y -> if x != y then raise Not_found) l l' ; true
        with
          | Not_found | Invalid_argument _ -> false
        end
    | Plus l, Plus l' ->
        begin try
          List.iter2 (fun x y -> if x != y then raise Not_found) l l' ; true
        with
          | Not_found | Invalid_argument _ -> false
        end
    | If (a,b,t,e), If (a',b',t',e') ->
        a == a' && b == b' && t == t' && e == e'
    | Input (c,x,p), Input (c',x',p') ->
        c = c' && x = x' && p == p'
    | Output (c,t,p), Output (c',t',p') ->
        c = c' && t = t' && p == p'
    | Bottom i, Bottom j -> i = j
    | _ -> false

  let hash x =
    Hashtbl.hash
      (match x with
         | Zero -> Zero
         | Par l -> Par (List.map (fun x -> x.id) l)
         | Plus l -> Plus (List.map (fun x -> x.id) l)
         | If (a,b,t,e) -> If (Term_.hash a, Term_.hash b,t.id,e.id)
         | Input (c,t,p) -> Input (c,Term_.hash t,p.id)
         | Output (c,t,p) -> Output (c,Term_.hash t,p.id)
         | Bottom i -> Bottom i)

end

module HProc = Hashtbl.Make(PProc)

let new_id =
  let c = ref 0 in
    fun () -> incr c ; !c

let h = HProc.create 257

let hashcons c =
  try HProc.find h c with
    | Not_found ->
        let p = { id = new_id () ; contents = c } in
          HProc.add h c p ;
          p

(** Smart constructors *)

let zero = hashcons Zero
let bottom i = hashcons (Bottom i)
let input c v p = hashcons (Input (c,v,p))
let output c t p = hashcons (Output (c,t,p))
let par l =
  match List.filter (function { contents } -> contents <> Zero) l with
    | [] -> zero
    | [p] -> p
    | l ->
        let l = List.sort compare l in
          hashcons (Par l)
let plus l =
  let l = List.filter (function { contents } -> contents <> Zero) l in
    match l with
      | [] -> zero
      | [x] -> x
      | l ->
          let l = List.sort_uniq compare l in
            hashcons (Plus l)
let if_eq a b t e = hashcons (If (a,b,t,e))
let if_neq a b t e = hashcons (If (a,b,e,t))

(** Substitution *)

let subst p x y = assert false

(** Printing *)

let rec pp ch = function
  | { contents = Input (c,_,_) } -> Format.fprintf ch "in(%d).." (Channel.to_int c)
  | { contents = Output (c,_,_) } -> Format.fprintf ch "out(%d).." (Channel.to_int c)
  | { contents = Par l } ->
      Format.fprintf ch "par(" ;
      let rec aux ch = function
        | [] -> Format.fprintf ch ")"
        | [p] -> Format.fprintf ch "%a)" pp p
        | p::tl -> Format.fprintf ch "%a;%a" pp p aux tl
      in aux ch l
  | { contents = If (a,b,t,e) } ->
      Format.fprintf ch "if(%s,%s,...)"
        (Term_.to_string a)
        (Term_.to_string b)
  | { contents = Zero } -> Format.fprintf ch "0"
  | { contents = Bottom i } -> Format.fprintf ch "⊥%d" i
  | _ -> Format.fprintf ch "?"

(** Tests *)

let () =
  Check.add_suite
    ("PProc",
     [ "Basic equalities test", `Quick,
       (fun () ->
          let p = output (Channel.of_int 0) (Term_.ok ()) zero in
          let p1 = Plus [p;p] in
          let p2 = Plus [p;p] in
            Alcotest.(check bool) "physically different" (p1 != p2) true ;
            Alcotest.(check bool) "structurally equal" (p1 = p2) true ;
            Alcotest.(check bool) "PTerm_.equal" (PProc.equal p1 p2) true) ])

let () =
  Check.add_suite
    ("Process_",
     [ "Singleton sum", `Quick,
       (fun () ->
          let p = output (Channel.of_int 0) (Term_.ok ()) zero in
          let q = plus [p] in
            Alcotest.(check bool) "equal" true (equal q p)) ;
       "Ordered idempotent sums", `Quick,
       (fun () ->
          let p = output (Channel.of_int 0) (Term_.ok ()) zero in
          let q = output (Channel.of_int 1) (Term_.ok ()) zero in
          let p1 = plus [p;q] in
          let p2 = plus [q;p;q] in
            Alcotest.(check bool) "equal" true (equal p1 p2)) ])
