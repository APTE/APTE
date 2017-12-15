open Channel
open Frame

type term = Term.term
type formula = Formula.formula

type ('a,'t,'f) _proc =
  | Zero
  | Par of 'a list
  | Plus of 'a list
  | If of 'f * 'a * 'a
  | Input of channel * 't * 'a
  | Output of channel * 't * 'a
  | Bottom of int

type proc = { id : int; contents : (proc,term,formula) _proc; }
type t = proc
	   
let equal x y = x == y
let compare x y = compare x.id y.id
let hash x = x.id
	       
(** HashedType instance for processes whose subprocesses are hash-consed. *)
module PProc : Hashtbl.HashedType with type t = (proc,term,formula) _proc = struct

  type t = (proc,term,formula) _proc
									     
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
    | If (f,t,e), If (f',t',e') ->
        f=f' && t == t' && e == e'
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
       | If (f,t,e) -> If (f.Formula.id,t.id,e.id)
       | Input (c,t,p) -> Input (c,Term.hash t,p.id)
       | Output (c,t,p) -> Output (c,Term.hash t,p.id)
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
let if_form f t e = hashcons (If (f,t,e))

let if_eq t1 t2 t e = hashcons (If (Formula.form_eq t1 t2,t,e))
let if_neq t1 t2 t e = hashcons (If (Formula.form_neq t1 t2,t,e))

(** Substitution *)
let rec subst p x y = match p.contents with
  | Zero -> p
  | Input (c,z,q) ->
      if Term.equal x z then p else
        (* The distinction between var and invar
         * prevents variable capture here ---
         * we always use subst for an invar. *)
        input c z (subst q x y)
  | Output (c,t,q) ->
      output c (Term.subst t x y) (subst q x y)
  | Par l ->
      par (List.map (fun q -> subst q x y) l)
  | Plus l ->
      plus (List.map (fun q -> subst q x y) l)
  | If (f,t,e) ->
     if_form (Formula.subst f x y)
             (subst t x y)
             (subst e x y)
  | Bottom _ -> assert false

type ('a,'b) trans_table =
    { output : 'a list Channel.Map.t ;
      input : 'b list Channel.Map.t }

(** Tests over subprocesses *)

let rec for_all_plus pred = function
  | { contents = Plus l } -> List.for_all (fun p -> for_all_plus pred p) l
  | p -> pred p

let rec exists_par pred = function
  | { contents = Par l } -> List.exists (fun p -> exists_par pred p) l
  | p -> pred p

(** Pre-transitions for process free of conditionals and bottoms at toplevel *)
let transitions proc =
  let outputs = ref [] in
  let inputs = ref [] in
  let add_input c f = inputs := (c,f) :: !inputs in
  let add_output c t p = outputs := (c,(t,p)) :: !outputs in
  let rec aux context proc = match proc.contents with
    | Zero -> ()
    | Input (c,x,p) ->
        add_input c
          (fun y ->
             par (subst p x y :: context))
    | Output (c,t,p) ->
        add_output c t (par (p :: context))
    | Plus l ->
        List.iter (aux context) l
    | Par l ->
        let rec try_all context = function
          | [] -> ()
          | p::l ->
              aux (List.rev_append l context) p ;
              try_all (p::context) l
        in try_all context l
    | If (f,t,e) -> assert false
    | Bottom i -> assert false
  in
  let () = aux [] proc in
    { Channel.
      outputs = Channel.Map.of_elem_list !outputs ;
      inputs = Channel.Map.of_elem_list !inputs }

(** Printing *)		     
let rec pp ch = function
  | { contents = Input (c,x,q) } ->
      if equal q zero then
        Format.fprintf ch "in(%c,%a)"
          (Channel.to_char c)
          Term.pp x
      else
        Format.fprintf ch "in(%c,%a).%a"
          (Channel.to_char c)
          Term.pp x
          pp q
  | { contents = Output (c,t,q) } ->
      if equal q zero then
        Format.fprintf ch "out(%c,%a)"
          (Channel.to_char c)
          Term.pp t
      else
        Format.fprintf ch "out(%c,%a).%a"
          (Channel.to_char c)
          Term.pp t
          pp q
  | { contents = Par l } ->
      Format.fprintf ch "@[<1>(" ;
      let rec aux ch = function
        | [] -> Format.fprintf ch ")@]"
        | [p] -> Format.fprintf ch "%a)@]" pp p
        | p::tl -> Format.fprintf ch "%a|@,%a" pp p aux tl
      in aux ch l
  | { contents = Plus l } ->
      Format.fprintf ch "@[<1>(" ;
      let rec aux ch = function
        | [] -> Format.fprintf ch ")@]"
        | [p] -> Format.fprintf ch "%a)@]" pp p
        | p::tl -> Format.fprintf ch "%a+@,%a" pp p aux tl
      in aux ch l
  | { contents = If (f,t,e) } ->
      if equal e zero then
        Format.fprintf ch "[%a].%a"
		       Formula.pp f
		       pp t
      else if equal t zero then
        Format.fprintf ch "¬[%a].%a"
		       Formula.pp f
		       pp e
      else
        Format.fprintf ch "if [%a] then %a else %a"
		       Formula.pp f
		       pp t pp e
  | { contents = Zero } -> Format.fprintf ch "0"
  | { contents = Bottom i } -> Format.fprintf ch "⊥%d" i

(** Tests *)

let () =
  Check.add_suite
    ("PProc",
     [ "Basic equalities test", `Quick,
       (fun () ->
          let p = output (Channel.of_int 0) (Term.ok ()) zero in
          let p1 = Plus [p;p] in
          let p2 = Plus [p;p] in
            Alcotest.(check bool) "physically different" (p1 != p2) true ;
            Alcotest.(check bool) "structurally equal" (p1 = p2) true ;
            Alcotest.(check bool) "PTerm.equal" (PProc.equal p1 p2) true) ])

let nb_inputs tbl c =
  try List.length (Channel.Map.get tbl.Channel.inputs c) with
    | Not_found -> 0

let nb_outputs tbl c =
  try List.length (Channel.Map.get tbl.Channel.outputs c) with
    | Not_found -> 0

let nb_actions tbl c = nb_inputs tbl c + nb_outputs tbl c

let () =
  Check.add_suite
    ("Process",
     [ "Singleton sum", `Quick,
       (fun () ->
          let p = output (Channel.of_int 0) (Term.ok ()) zero in
          let q = plus [p] in
            Alcotest.(check bool) "equal" true (equal q p)) ;
       "Ordered idempotent sums", `Quick,
       (fun () ->
          let p = output (Channel.of_int 0) (Term.ok ()) zero in
          let q = output (Channel.of_int 1) (Term.ok ()) zero in
          let p1 = plus [p;q] in
          let p2 = plus [q;p;q] in
            Alcotest.(check bool) "equal" true (equal p1 p2)) ;
       "Transitions", `Quick,
       (fun () ->
          let c = Channel.c in
          let o = output c (Term.ok ()) zero in
          let io = input c (Term.var "x") o in
          let tbl = transitions io in
          Format.printf "io = %a\n" pp io ;
          Alcotest.(check int)
            "no transition for channel d"
            0
            (nb_actions tbl d) ;
          Alcotest.(check int)
            "one input for channel c"
            1
            (nb_inputs tbl c) ;
          Alcotest.(check int)
            "no output for channel c"
            0
            (nb_outputs tbl c) ;
          let p = List.hd (Channel.Map.get tbl.inputs c) in
          let p = p (Term.invar (Channel.of_int 0,0,Frame.empty)) in
          Format.printf "p = %a\n" pp p ;
          let tbl' = transitions p in
          Alcotest.(check int)
            "one in(0).out(0) trace"
            1
            (nb_outputs tbl' c) ;
          let _,q = List.hd (Channel.Map.get tbl'.outputs c) in
          Format.printf "q = %a\n" pp q ;
          let tbl'' = transitions q in
          Alcotest.(check int)
            "nothing on 0 after in(0).out(0)"
            0
            (nb_actions tbl'' c) ;
          Alcotest.(check bool)
            "process equals 0 after in(0).out(0)"
            true
            (equal zero q)
       ) ;
       "Substitution", `Quick,
       (fun () ->
          let phi = Frame.append Frame.empty c (Term.ok()) in
          Alcotest.(check bool)
            "correct result for subst"
            true
            (equal
               (output Channel.c (Term.invar (Channel.c,1,phi)) zero)
               (subst
                  (output Channel.c (Term.var "x") zero)
                  (Term.var "x")
                  (Term.invar (Channel.c,1,phi))))) ;
     ])
