(************************
***       Types       ***
*************************)

type quantifier =
  | Free
  | Existential
  | Universal

type symbol_cat =
  | Tuple
  | Constructor
  | Destructor of term list * term

and symbol =
  {
    name : string;
    arity : int;
    cat : symbol_cat
  }

and link =
  | NoLink
  | TLink of term
  | VLink of variable

and variable =
  {
    id_v : string;
    number_v : int;
    mutable link : link;
    quantifier : quantifier
  }

and name_status =
  | Public
  | Private

and name =
  {
    id_n : string;
    number_n : int;
    status : name_status
  }

and term =
  | Func of symbol * term list
  | Var of variable
  | Name of name

let compare_name n1 n2 =
  n2.number_n - n1.number_n

(*********************************
***    Variable generation     ***
**********************************)

let accumulator_variable = ref 0

let fresh_variable_from_id q id =
  let var = { id_v = id; number_v = !accumulator_variable; link = NoLink; quantifier = q } in
  accumulator_variable := !accumulator_variable + 1;
  var

let fresh_variable q = fresh_variable_from_id q "v"

let fresh_variable_from_var var = fresh_variable_from_id var.quantifier var.id_v

let rec fresh_variable_list q = function
  | 0 -> []
  | ar -> (fresh_variable q)::(fresh_variable_list q (ar-1))

let rec fresh_variable_list2 q = function
  | 0 -> []
  | ar -> (Var(fresh_variable q))::(fresh_variable_list2 q (ar-1))


(*********************************
***            Symbol          ***
**********************************)

(********* Built-in constructors ***********)

let senc = { name = "senc"; arity = 2; cat = Constructor }
let aenc = { name = "aenc"; arity = 2; cat = Constructor }
let pk = { name = "pk"; arity = 1; cat = Constructor }
let vk = { name = "vk"; arity = 1; cat = Constructor }
let sign = { name = "sign"; arity = 2; cat = Constructor }
let hash = { name = "hash"; arity = 1; cat = Constructor }

(********* Built-in destructors ************)

let sdec =
  let x = Var(fresh_variable_from_id Free "x")
  and y = Var(fresh_variable_from_id Free "y") in
  let arg = Func(senc,[x;y]) in

  { name = "sdec"; arity = 2; cat = Destructor([arg;y],x) }

let adec =
  let x = Var(fresh_variable_from_id Free "x")
  and y = Var(fresh_variable_from_id Free "y") in
  let arg = Func(aenc,[x;Func(pk,[y])]) in

  { name = "adec"; arity = 2; cat = Destructor([arg;y],x) }

let checksign =
  let x = Var(fresh_variable_from_id Free "x")
  and y = Var(fresh_variable_from_id Free "y") in
  let arg = Func(sign,[x;y]) in

  { name = "check"; arity = 2; cat = Destructor([arg;Func(vk,[y])],x) }

(********* Tuple ************)

let all_projection = Hashtbl.create 7

let nth_projection symb_tuple i = match symb_tuple.cat with
  | Tuple ->
      let ar = symb_tuple.arity in
      (Hashtbl.find all_projection ar).(i-1)
  | _ -> Debug.internal_error "[term.ml >> nth_projection] The function symbol should be a tuple"

let get_projections symb_tuple = match symb_tuple.cat with
  | Tuple -> Array.to_list (Hashtbl.find all_projection (symb_tuple.arity))
  | _ -> Debug.internal_error "[term.ml >> get_projections] The function symbol should be a tuple"

let all_constructors = ref [senc;aenc;pk;vk;sign;hash]

let all_tuple = ref []

let number_of_constructors = ref 6

(********* Addition ************)

let new_constructor ar s =
  let symb = { name = s; arity = ar; cat = Constructor } in
  all_constructors := symb::!all_constructors;
  number_of_constructors := !number_of_constructors + 1;
  symb

let new_projection tuple_symb i =
  let args = fresh_variable_list2 Free tuple_symb.arity in
  let x = List.nth args i in
  {
    name = (Printf.sprintf "proj_%d_of_%d" (i+1) tuple_symb.arity);
    arity = 1;
    cat = Destructor([Func(tuple_symb,args)],x)
  }

let get_tuple ar =
  try
    List.find (fun symb -> symb.arity = ar) !all_tuple
  with Not_found ->
    begin
      let symb = { name = ""; arity = ar; cat = Tuple } in
      all_constructors := symb::!all_constructors;
      all_tuple := symb::!all_tuple;
      number_of_constructors := !number_of_constructors + 1;

      let array_proj = Array.init ar (new_projection symb) in
      Hashtbl.add all_projection ar array_proj;
      symb
    end;;

ignore(get_tuple 2);;

(********* Symbols functions *********)

let is_equal_symbol sym_1 sym_2 =
  sym_1 == sym_2

let is_constructor sym =
  sym.cat = Constructor || sym.cat = Tuple

let is_tuple sym =
  sym.cat = Tuple

let is_destructor sym = match sym.cat with
  | Destructor(_,_) -> true
  | _ -> false

let get_arity sym = sym.arity

(********* Generation of fresh names *********)

let accumulator_name = ref 0

let fresh_name_from_id s id =
  let name = { id_n = id; number_n = !accumulator_name; status = s } in
  accumulator_name := !accumulator_name + 1;
  name

let fresh_name s = fresh_name_from_id s "n"

let fresh_name_from_name name = fresh_name_from_id name.status name.id_n

let name_bad = fresh_name_from_id Private "_not_secret"

(********* Generation of terms *********)

let term_of_variable var = Var(var)

let term_of_name name = Name(name)

let variable_of_term term = match term with
  | Var(var) -> var
  | _ -> Debug.internal_error "[term.ml >> variable_from_term] The term should be a variable"

let name_of_term term = match term with
  | Name(name) -> name
  | _ -> Debug.internal_error "[term.ml >> name_from_term] The term should be a name"

let apply_function symbol list_sons =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if (List.length list_sons) <> symbol.arity
    then Debug.internal_error (Printf.sprintf "[term.ml >> apply_function] The function %s has arity %d but is given %d terms" symbol.name symbol.arity (List.length list_sons))
  );
  (***[END DEBUG]***)

  Func(symbol,list_sons)

(********* Access Functions *********)

let top term = match term with
  | Func(s,_) -> s
  | _ -> Debug.internal_error "[terms.ml >> top] The term is not a function application"

let nth_args t i = match t with
  | Func(_,l) -> List.nth l (i-1)
  | _ -> Debug.internal_error "[terms.ml >> nth_args] The term is not a function application"

let get_args = function
  | Func(_,l) -> l
  | _ -> Debug.internal_error "[terms.ml >> get_args] The term is not a function application"

let get_quantifier v = v.quantifier

(********* Scanning Functions *********)
let rec is_closed = function
  | Func (_, tlist)  -> List.for_all is_closed tlist
  | Var _ -> false
  | Name _ -> true

let rec var_occurs var = function
  | Var(v) when v == var -> true
  | Var({link = TLink t; _}) -> var_occurs var t
  | Func(_,args) -> List.exists (var_occurs var) args
  | _ -> false

let rec var_occurs_list var_list = function
  | Var(v) when List.exists (fun var -> v == var) var_list -> true
  | Func(_,args) -> List.exists (var_occurs_list var_list) args
  | _ -> false

let rec exists_var q = function
  | Var(v) when v.quantifier = q -> true
  | Func(_,args) -> List.exists (exists_var q) args
  | _ -> false

let rec for_all_var q = function
  | Var(v) when v.quantifier = q -> true
  | Func(_,args) -> List.for_all (for_all_var q) args
  | _ -> false

let rec exists_name_with_status s = function
  | Name(n) when n.status = s -> true
  | Func(_,args) -> List.exists (exists_name_with_status s) args
  | _ -> false

let rec exists_name = function
  | Name(_) -> true
  | Func(_,args) -> List.exists exists_name args
  | _ -> false

let rec is_equal_term t1 t2 = match t1,t2 with
  | Var(v1),Var(v2) when v1 == v2 -> true
  | Name(n1),Name(n2) when n1 == n2 -> true
  | Func(f1,args1), Func(f2,args2) when f1 == f2 -> List.for_all2 is_equal_term args1 args2
  | _,_ -> false

let rec is_equal_and_closed_term t1 t2 = match t1,t2 with
  | Name(n1),Name(n2) when n1 == n2 -> true
  | Func(f1,args1), Func(f2,args2) when f1 == f2 -> List.for_all2 is_equal_and_closed_term args1 args2
  | _,_ -> false

let is_equal_name n1 n2 = n1 == n2

let rec name_occurs n = function
  | Name(n') when is_equal_name n n' -> true
  | Func(_,args) -> List.exists (name_occurs n) args
  | _ -> false

let is_variable t = match t with
  | Var(_) -> true
  | _ -> false

let is_name t = match t with
  | Name(_) -> true
  | _ -> false

let is_name_status s t = match t with
  | Name(n) when n.status = s -> true
  | _ -> false

let is_function t = match t with
  | Func(_,_) -> true
  | _ -> false

let rec is_constructor_term = function
  | Func({cat = Destructor _; _},_) -> false
  | Func(_,args) -> List.for_all is_constructor_term args
  | _ -> true

(********* Iterators *********)

let fold_left_args f_acc acc = function
  | Func(_,l) -> List.fold_left f_acc acc l
  | _ -> Debug.internal_error "[terms.ml >> fold_left_args] The term is not a function application"

let fold_right_args f_acc term acc = match term with
  | Func(_,l) -> List.fold_right f_acc l acc
  | _ -> Debug.internal_error "[terms.ml >> fold_right_args] The term is not a function application"

let map_args f_map = function
  | Func(_,l) -> List.map f_map l
  | _ -> Debug.internal_error "[terms.ml >> map_args] The term is not a function application"

let fold_left_args2 f_acc acc term l = match term with
  | Func(_,l_args) ->
      List.fold_left2 f_acc acc l_args l
  | _ -> Debug.internal_error "[terms.ml >> fold_left_args2] The term is not a function application"

(*********************************
***        Substitution        ***
**********************************)

type substitution = (variable * term) list

let identity = []

let is_identity subst = subst = []

let create_substitution var term = [var,term]

let filter_domain f_test subst = List.filter (fun (v,_) -> f_test v) subst

let rec apply_substitution_on_term term = match term with
  | Func(f,args) -> Func(f, List.map apply_substitution_on_term args)
  | Var(t) ->
      begin match t.link with
        | NoLink -> term
        | TLink t' -> t'
        | _ -> Debug.internal_error "[term.ml >> apply_substitution_on_term] Unexpected link"
      end
  | _ -> term

let apply_substitution subst elt f_iter_elt =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if List.exists (fun (v,_) -> v.link <> NoLink) subst
    then Debug.internal_error "[term.ml >> apply_substitution] Variables in the domain should not be linked"
  );
  (***[END DEBUG]***)

  (* Link the variables of the substitution *)
  List.iter (fun (v,t) -> v.link <- (TLink t)) subst;

  try
    (* Apply the substitution on the element *)
    let new_elt = f_iter_elt elt apply_substitution_on_term in

    (* Unlink the variables of the substitution *)
    List.iter (fun (v,_) -> v.link <- NoLink) subst;

    new_elt
  with exc ->
    (* Unlink the variables of the substitution *)
    List.iter (fun (v,_) -> v.link <- NoLink) subst;
    raise exc

let apply_substitution_on_term_change_detected term =

  let change_detected = ref false in

  let rec go_through term = match term with
    | Func(f,args) -> Func(f, List.map go_through args)
    | Var(t) ->
        begin match t.link with
        | NoLink -> term
        | TLink t' ->
            change_detected := true;
            t'
        | _ -> Debug.internal_error "[term.ml >> apply_substitution_on_term_change_detected] Unexpected link"
      end
    | _ -> term
  in

  let term' = go_through term in
  (!change_detected,term')

let apply_substitution_change_detected subst elt f_iter_elt =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if List.exists (fun (v,_) -> v.link <> NoLink) subst
    then Debug.internal_error "[term.ml >> apply_substitution] Variables in the domain should not be linked"
  );
  (***[END DEBUG]***)

  (* Link the variables of the substitution *)
  List.iter (fun (v,t) -> v.link <- (TLink t)) subst;

  try
    (* Apply the substitution on the element *)
    let new_elt = f_iter_elt elt apply_substitution_on_term_change_detected in

    (* Unlink the variables of the substitution *)
    List.iter (fun (v,_) -> v.link <- NoLink) subst;

    new_elt
  with exc ->
    (* Unlink the variables of the substitution *)
    List.iter (fun (v,_) -> v.link <- NoLink) subst;
    raise exc

let compose subst1 subst2 =
  (***[BEGIN DEBUG]***)
  if List.exists (fun (v1,_) -> List.exists (fun (v2,_) -> v1 == v2) subst1) subst2
  then Debug.internal_error "[term.ml >> compose] The domains of the two substitutions are not disjoint";
  (***[END DEBUG]***)

  let subst1' = apply_substitution subst2 subst1 (fun l f_apply -> List.map (fun (v,t) -> (v,f_apply t)) l) in
  subst1'@subst2

let equations_of_substitution = List.map (fun (v,t) -> Var v,t)

(***********************************
***           Renaming           ***
************************************)

(******* Variable renaming *********)

let linked_variables_renaming = ref []

let vlink var var' =
  var.link <- (VLink var');
  linked_variables_renaming := var::(!linked_variables_renaming)

let vcleanup () =
  List.iter (fun var -> var.link <- NoLink) !linked_variables_renaming;
  linked_variables_renaming := []

let rec renaming_term quantifier = function
  | Var(v) ->
      begin match v.link with
        | VLink(v') -> Var(v')
        | NoLink ->
            let v' = fresh_variable_from_id quantifier v.id_v in
            vlink v v';
            Var(v')
        | _ -> Debug.internal_error "[term.ml >> renaming_term] Unexpected link"
      end
  | Func(f,args) -> Func(f, List.map (renaming_term quantifier) args)
  | term -> term

let rec rename var_assoc name_assoc = function
  | Var(v) ->
      begin try
        Var(List.assoc v var_assoc)
      with
        Not_found -> Var(v)
      end
  | Name(n) ->
      begin try
        Name(List.assoc n name_assoc)
      with
        Not_found -> Name(n)
      end
  | Func(f,args) -> Func(f,List.map (rename var_assoc name_assoc) args)

(*********************************
***        Unification         ***
**********************************)

exception Not_unifiable

(****** Variable manipulation *******)

let linked_variables = ref []

let link var term =
  var.link <- (TLink term);
  linked_variables := var::(!linked_variables)

let rec follow_link = function
  | Func(f,args) -> Func(f,List.map follow_link args)
  | Var({link = TLink t;_}) -> follow_link t
  | term -> term

let follow_link_change_detected term =
  let change_detected = ref false in

  let rec go_through = function
    | Func(f,args) -> Func(f,List.map go_through args)
    | Var({link = TLink t;_}) -> change_detected := true; go_through t
    | term -> term
  in

  let result = go_through term in
  !change_detected,result

let cleanup () =
  List.iter (fun var -> var.link <- NoLink) !linked_variables;
  linked_variables := []

(******* Syntactic unification *******)

let rec unify_term t1 t2 = match t1,t2 with
  | Var(v1), Var(v2) when v1 == v2 -> ()
  | Var({link = TLink t ; _}), _ -> unify_term t t2
  | _, Var({link = TLink t; _}) -> unify_term t1 t
  | Var(v1),Var(v2) ->
      if v1.quantifier = Universal || (v1.quantifier = Existential && v1.quantifier = Free)
      then link v1 t2
      else link v2 t1
  | Var(v1), _ -> if var_occurs v1 t2 then raise Not_unifiable else link v1 t2
  | _, Var(v2) -> if var_occurs v2 t1  then raise Not_unifiable else link v2 t1
  | Name(n1), Name(n2) when n1 == n2 -> ()
  | Func(f1,args1), Func(f2,args2) ->
      if f1 == f2 then List.iter2 unify_term args1 args2 else raise Not_unifiable
  | _,_ -> raise Not_unifiable

let unify eq_list =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if !linked_variables <> []
    then Debug.internal_error "[term.ml >> unify] The list of linked variables should be empty"
  );
  (***[END DEBUG]***)

  try
    List.iter (fun (t1,t2) -> unify_term t1 t2) eq_list;
    let subst = List.map (fun var -> (var,follow_link (Var var))) !linked_variables in
    cleanup ();
    subst
  with
    | exc ->
      cleanup ();
      raise exc

let is_unifiable eq_list =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if !linked_variables <> []
    then Debug.internal_error "[term.ml >> unify] The list of linked variables should be empty"
  );
  (***[END DEBUG]***)

  try
    List.iter (fun (t1,t2) -> unify_term t1 t2) eq_list;
    cleanup ();
    true
  with
    | Not_unifiable ->
      cleanup ();
      false

let unify_and_apply eq_list elt f_iter_elt =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if !linked_variables <> []
    then Debug.internal_error "[term.ml >> unify_and_apply] The list of linked variables should be empty"
  );
  (***[END DEBUG]***)

  try
    List.iter (fun (t1,t2) -> unify_term t1 t2) eq_list;
    let new_elt = f_iter_elt elt follow_link in
    cleanup ();
    new_elt
  with
    | exc ->
        cleanup ();
        raise exc

let unify_and_apply_change_detected eq_list elt f_iter_elt =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if !linked_variables <> []
    then Debug.internal_error "[term.ml >> unify_and_apply_change_detected] The list of linked variables should be empty"
  );
  (***[END DEBUG]***)
  try
    List.iter (fun (t1,t2) -> unify_term t1 t2) eq_list;

    let new_elt = f_iter_elt elt follow_link_change_detected in
    cleanup ();
    new_elt
  with
    | exc ->
        cleanup ();
        raise exc

(********************************************************
***        Unification modulo rewriting system        ***
*********************************************************)

let rec rewrite_term quantifier = function
  | Func(f1,args) ->
      begin match f1.cat with
        | Constructor | Tuple -> Func(f1,List.map (rewrite_term quantifier) args)
        | Destructor(lhs,rhs) ->
            let args' = List.map (rewrite_term quantifier) args in

            (***[BEGIN DEBUG]***)
            if !linked_variables_renaming <> []
            then Debug.internal_error "[term.ml >> rewrite_term] The list of linked variables for renaming should be empty";
            (***[END DEBUG]***)

            let lhs' = List.map (renaming_term quantifier) lhs in
            let rhs' = renaming_term quantifier rhs in

            vcleanup ();

            List.iter2 unify_term lhs' args';

            rhs'
      end
   | term -> term

let unify_modulo_rewrite_rules t_list =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if !linked_variables <> []
    then Debug.internal_error "[term.ml >> unify_modulo_rewrite_rules] The list of linked variables should be empty"
  );
  (***[END DEBUG]***)
  try
    List.iter (fun (t1,t2) ->
      let t1' = rewrite_term Existential t1 in
      let t2' = rewrite_term Existential t2 in
      unify_term t1' t2'
    ) t_list;

    let subst = List.map (fun v -> (v,follow_link (Var(v)))) !linked_variables in
    cleanup ();
    subst
  with
    | exc ->
      cleanup ();
      raise exc

let unify_modulo_rewrite_rules_and_apply t_list elt f_iter_elt =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if !linked_variables <> []
    then Debug.internal_error "[term.ml >> unify_modulo_rewrite_rules_and_apply] The list of linked variables should be empty"
  );
  (***[END DEBUG]***)
  try
    List.iter (fun (t1,t2) ->
      let t1' = rewrite_term Existential t1 in
      let t2' = rewrite_term Existential t2 in
      unify_term t1' t2'
    ) t_list;

    let new_elt = f_iter_elt elt follow_link in
    cleanup ();
    new_elt
  with
    | exc ->
      cleanup ();
      raise exc

(***********************************
***       Formula on terms       ***
************************************)

type formula =
  | Top
  | Bottom
  | Forall of (term * term) list

let top_formula = Top

let bottom_formula = Bottom

let is_bottom = function
  | Bottom -> true
  | _ -> false

let is_top = function
  | Top -> true
  | _ -> false


let create_inequation t1 t2 = Forall([t1,t2])

let create_disjunction_inequation l = Forall(l)

let is_in_formula t1 t2 = function
  | Top -> false
  | Bottom -> false
  | Forall(neq_list) ->
      List.exists (fun (u1,u2) ->
        (is_equal_term t1 u1 && is_equal_term t2 u2) ||
        (is_equal_term t1 u2 && is_equal_term t2 u1)
      ) neq_list


(*********** Iterators **********)

let iter_inequation_formula f_apply = function
  | Top -> ()
  | Bottom -> ()
  | Forall l -> List.iter (fun (t1,t2) -> f_apply t1 t2) l

let map_term_formula formula f_apply = match formula with
  | Top -> Top
  | Bottom -> Bottom
  | Forall(neq_list) -> Forall(List.map (fun (t1,t2) -> f_apply t1, f_apply t2) neq_list)

let map_term_formula_change_detected formula f_apply =
  let change_detected = ref false in

  let result = match formula with
    | Top -> Top
    | Bottom -> Bottom
    | Forall(neq_list) ->
        Forall(
          List.map (fun (t1,t2) ->
            let change_1,t1' = f_apply t1
            and change_2,t2' = f_apply t2 in

            if change_1 || change_2
            then change_detected := true;

            t1',t2'
          ) neq_list
        )
  in
  !change_detected,result

let find_and_apply_formula f_test f_apply f_no_result = function
  | Bottom | Top -> f_no_result ()
  | Forall(neq_list) ->
      let rec go_through = function
        | [] -> f_no_result ()
        | (t1,t2)::_ when f_test t1 t2 -> f_apply t1 t2
        | _::q -> go_through q
      in
      go_through neq_list

(*********** Simplification **********)

let simplify_formula formula =
  let prev_linked_variables = !linked_variables in
  linked_variables := [];

  let result = match formula with
    | Top -> Top
    | Bottom -> Bottom
    | Forall(neq_list) ->
        try
          List.iter (fun (t1,t2) -> unify_term t1 t2) neq_list;

          (***[BEGIN DEBUG]***)
          Debug.low_debugging (fun () ->
            if List.exists (fun v ->
              if v.quantifier <> Universal
              then
                begin match v.link with
                  |TLink (Var v') when v'.quantifier = Universal -> true
                  |_ -> false
                end
              else false
              ) !linked_variables
            then Debug.internal_error "[term.ml >> simplify_formula] An existential or free variable cannot be linked to a universal variable"
          );
          (***[END DEBUG]***)

          let not_univ_var_linked = List.filter (fun v -> not (v.quantifier = Universal)) !linked_variables in

          let simplified_formula =
            if not_univ_var_linked = []
            then Bottom
            else Forall(List.map (fun v -> (Var(v),follow_link (Var(v)))) not_univ_var_linked)
          in
          simplified_formula
        with
          Not_unifiable -> Top
  in

  cleanup ();
  linked_variables := prev_linked_variables;
  result

let rec semi_unify_term ref_list t1 t2 = match t1,t2 with
  | Func(f1,args1),Func(f2,args2) ->
      if f1 != f2
      then raise Not_unifiable
      else List.iter2 (semi_unify_term ref_list) args1 args2

  | Var v1,Var v2 when v1 == v2 -> ()
  | Name n1,Name n2 when n1 == n2 -> ()

  (* Rule Nelim1 *)
  | Var {link = TLink t; quantifier = Universal; _},t2 -> semi_unify_term ref_list t t2
  | t1,Var {link = TLink t; quantifier = Universal; _} -> semi_unify_term ref_list t1 t
  | Var v,t2 when v.quantifier = Universal ->  link v t2
  | t1,Var v when v.quantifier = Universal ->  link v t1

  | Var v,t2 ->
      (* We unify the term fully to check if the rule Nneq1 can be applied.
         Note that even if we unify the term, we do not instantiate them. *)
      unify_term (Var v) t2;
      ref_list := (Var v,t2)::!ref_list
  | t1,Var v ->
      (* We order the variable always on the left side of the inequation *)
      unify_term t1 (Var v);
      ref_list := (Var v,t1)::!ref_list

  | _,_-> raise Not_unifiable

let simplify_formula_phase_2 formula =
  let prev_linked_variables = !linked_variables in
  linked_variables := [];

  let list_neq = ref [] in

  let result = match formula with
    | Top -> Top
    | Bottom -> Bottom
    | Forall(neq_list) ->
        try
          List.iter (fun (t1,t2) -> semi_unify_term list_neq t1 t2) neq_list;
          if !list_neq = [] then Bottom else Forall(!list_neq)
        with Not_unifiable -> Top
  in

  cleanup ();
  linked_variables := prev_linked_variables;
  result

let simplify_formula_modulo_rewrite_rules formula =
  let prev_linked_variables = !linked_variables in
  linked_variables := [];

  let result = match formula with
    | Top -> Top
    | Bottom -> Bottom
    | Forall(neq_list) ->
        try
          List.iter (fun (t1,t2) ->
            let t1' = rewrite_term Universal t1 in
            let t2' = rewrite_term Universal t2 in
            unify_term t1' t2'
          ) neq_list;

          (***[BEGIN DEBUG]***)
          Debug.low_debugging (fun () ->
            if List.exists (fun v ->
              if v.quantifier <> Universal
              then
                begin match v.link with
                  |TLink (Var v') when v'.quantifier = Universal -> true
                  |_ -> false
                end
              else false
             ) !linked_variables
            then Debug.internal_error "[term.ml >> simplify_formula_modulo_rewrite_rules] An existential or free variable cannot be linked to a universal variable"
          );
          (***[END DEBUG]***)

          let not_uni_var_linked = List.filter (fun v -> not (v.quantifier = Universal)) !linked_variables in

          if not_uni_var_linked = []
          then Bottom
          else Forall(List.map (fun v -> (Var(v),follow_link (Var(v)))) not_uni_var_linked)
        with Not_unifiable -> Top
  in

  cleanup ();
  linked_variables := prev_linked_variables;
  result


(***********************************
***         Rewrite rules        ***
************************************)

let fresh_rewrite_rule symbol = match symbol.cat with
  | Constructor | Tuple -> Debug.internal_error "[Term.ml >> fresh_rewrite_rule] No rewrite rule is associated to a constructor"
  | Destructor(arg,r) ->
      (***[BEGIN DEBUG]***)
      Debug.low_debugging (fun () ->
        if !linked_variables_renaming <> []
        then Debug.internal_error "[term.ml >> fresh_rewrite_rule] The list of linked variables for renaming should be empty"
      );
      (***[END DEBUG]***)
      let arg' = List.map (renaming_term Existential) arg in
      let r' = renaming_term Existential r in
      vcleanup ();
      arg', r'

let link_destruc_construc s_dest s_cons = match s_cons.cat with
  | Constructor ->
      (s_dest == sdec && s_cons == senc) ||
      (s_dest == adec && s_cons == aenc) ||
      (s_dest == checksign && s_cons == sign)
  | _ -> Debug.internal_error "[term.ml >> link_destruc_construc] The second symbol should not be a destructor or a tuple"

let constructor_to_destructor s_cons = match s_cons.cat with
  | Tuple -> Debug.internal_error "[term.ml >> constructor_to_destructor] The function symbol should not correspond to a tupple"
  | Constructor ->
      if s_cons == senc then sdec
      else if s_cons == aenc then adec
      else if s_cons == sign then checksign
      else Debug.internal_error "[term.ml >> constructor_to_destructor] The function symbol should correspond to one associted with a rewrite rule"
  | _ -> Debug.internal_error "[term.ml >> constructor_to_destructor] The function symbol should be a constructor"

(***********************************
***       Display function       ***
************************************)

let display_symbol_without_arity f = f.name

let display_symbol_with_arity f = Printf.sprintf "%s/%d" f.name f.arity

let display_name n = Printf.sprintf "N%s_%d" n.id_n n.number_n

let display_variable v = Printf.sprintf "V%s_%d" v.id_v v.number_v

let rec display_list_term = function
  | [] -> ""
  | [t] -> display_term t
  | t::q -> Printf.sprintf "%s,%s" (display_term t) (display_list_term q)

and display_term = function
  | Var(v) -> display_variable v
  | Name(n) -> display_name n
  | Func(f_symb,_) when f_symb.arity = 0 ->
      Printf.sprintf "%s" f_symb.name
  | Func(f_symb,args) when f_symb.cat = Tuple ->
      Printf.sprintf "(%s)" (display_list_term args)
  | Func(f_symb,args) ->
      Printf.sprintf "%s(%s)" f_symb.name (display_list_term args)

let rec get_universal_var var_list  = function
  | Name(_) -> ()
  | Var(v) when v.quantifier = Universal ->
      if not (List.exists (fun v' -> v = v') !var_list)
      then var_list := v::!var_list
  | Var(_) -> ()
  | Func(_,args) -> List.iter (get_universal_var var_list) args

let rec display_disjunction_neq = function
  | [] -> ""
  | [t1,t2] -> Printf.sprintf "%s <> %s" (display_term t1) (display_term t2)
  | (t1,t2)::q -> Printf.sprintf "%s <> %s \\/ %s" (display_term t1) (display_term t2) (display_disjunction_neq q)

let display_formula = function
  | Top -> "top"
  | Bottom -> "bottom"
  | Forall(neq_list) ->
      let uni_var = ref [] in
      List.iter (fun (t1,t2) ->
        get_universal_var uni_var t1;
        get_universal_var uni_var t2
      ) neq_list;
      if !uni_var = []
      then display_disjunction_neq neq_list
      else Printf.sprintf "Forall %s. %s" (display_list_term (List.map (fun v-> Var(v)) !uni_var)) (display_disjunction_neq neq_list)


(***********************************
***        Mapping function      ***
************************************)

module IntComp =
struct
  type t = int
  let compare = compare
end

module IntMap = Map.Make(IntComp)

module VariableMap =
struct
  type 'a map = 'a IntMap.t

  let empty = IntMap.empty

  let is_empty = IntMap.is_empty

  let add v = IntMap.add v.number_v

  let find v = IntMap.find v.number_v

  let mem v = IntMap.mem v.number_v

  let display f map =
    Printf.printf "VariableMap = { ";
    IntMap.iter (fun t elt -> Printf.printf "%d -> %s; " t (f elt)) map;
    Printf.printf "}"
end
