module Term = Frame.Term

type term = Term.t
	      
type ('a,'t) _formula =
  | Eq of 't * 't
  | Neq of 't * 't
  | And of 'a * 'a
  | Or of 'a * 'a
		 
type formula = {id : int; contents : (formula,term) _formula; }
type t = formula
	   
let equal x y = x == y
let compare x y = compare x.id y.id
let hash x = x.id
	       
(** HashedType instance for formula whose subformulas are hash-consed. *)
module PForm : Hashtbl.HashedType with type t = (formula,term) _formula = struct
						
  type t = (formula,term) _formula

  let equal f1 f2 = match f1,f2 with
    | Eq(t1,t2), Eq (t1',t2') -> Term.equal t1 t1' && Term.equal t2 t2'
    | Neq(t1,t2), Neq (t1',t2') -> Term.equal t1 t1' && Term.equal t2 t2'
    | And (f1,f2), And(f1',f2') -> f1==f1' && f2=f2'
    | Or (f1,f2), Or(f1',f2') -> f1==f1' && f2=f2'
    | _ -> false
	     
  let hash x =
    Hashtbl.hash
      (match x with
       | Eq (t1,t2) -> Eq (Term.hash t1, Term.hash t2)
       | Neq (t1,t2) -> Neq (Term.hash t1, Term.hash t2)
       | And (f1,f2) -> And (f1.id, f2.id)
       | Or (f1,f2) -> Or (f1.id, f2.id))
end

module HForm = Hashtbl.Make(PForm)
let new_id =
  let c = ref 0 in
  fun () -> incr c ; !c

let h = HForm.create 100

		     
let hashcons f =
  try HForm.find h f with
  | Not_found ->
     let f2 = { id = new_id () ; contents = f } in
     HForm.add h f f2 ;
     f2
       
(** Printing *)		     
let rec pp ch = function
  | {contents = Eq (t1,t2)} -> Format.fprintf ch "%a=%a" Term.pp t1 Term.pp t2
  | {contents = Neq (t1,t2)} -> Format.fprintf ch "%a≠%a" Term.pp t1 Term.pp t2
  | {contents = And (f1,f2)} -> Format.fprintf ch "(%a)∧(%a)" pp f1 pp f2
  | {contents = Or (f1,f2)} -> Format.fprintf ch "(%a)∨(%a)" pp f1 pp f2

(** Smart constructors providing hash consing *)
let form_eq t1 t2 = hashcons (Eq (t1,t2))
let form_neq t1 t2 = hashcons (Neq (t1,t2))
let form_and f1 f2 = hashcons (And (f1,f2))
let form_or f1 f2 = hashcons (Or (f1,f2))

(** Substitution *)
let rec subst f x y = match f.contents with
  | Eq (t1,t2) ->  form_eq (Term.subst t1 x y) (Term.subst t2 x y)
  | Neq (t1,t2) ->  form_neq (Term.subst t1 x y) (Term.subst t2 x y)
  | And (f1,f2) -> form_and (subst f1 x y) (subst f2 x y)
  | Or (f1,f2) -> form_or  (subst f1 x y) (subst f2 x y)
