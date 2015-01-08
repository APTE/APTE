(***************************
***         Recipe       ***
****************************)

type link = 
  | NoLink
  | RLink of recipe
  | CLink of context 

and variable = 
  {
    id : string;
    number : int;
    support : int;
    mutable link : link;
    free : bool
  }
  
(* An axiom is defined by its support. Its id will always be "ax". *)
and axiom = int

and recipe = 
  | Var of variable
  | Axiom of axiom
  | Func of Term.symbol * recipe list
  
and path = (Term.symbol list) * axiom
  
and context = 
  | CVar of variable
  | CPath of path
  | CFunc of Term.symbol * context list

(******** Fresh function ********)

let accumulator_variable = ref 0

let fresh_variable_from_id id support = 
  let var = 
    { 
      id = id;
      number = !accumulator_variable;
      support = support;
      link = NoLink;
      free = false
    }
  in
  accumulator_variable := !accumulator_variable + 1;
  var
  
let fresh_free_variable_from_id id support = 
  let var = 
    { 
      id = id;
      number = !accumulator_variable;
      support = support;
      link = NoLink;
      free = true
    }
  in
  accumulator_variable := !accumulator_variable + 1;
  var

let fresh_free_variable support = fresh_free_variable_from_id "X" support  
  
let fresh_variable support = fresh_variable_from_id "X" support

let axiom support = support

let rec fresh_variable_list arity support = match arity with
  | 0 -> []
  | ar -> (fresh_variable support)::(fresh_variable_list (ar-1) support)
  
let rec fresh_free_variable_list arity support = match arity with
  | 0 -> []
  | ar -> (fresh_free_variable support)::(fresh_free_variable_list (ar-1) support)
  
let rec fresh_variable_list2 arity support = match arity with
  | 0 -> []
  | ar -> (Var(fresh_variable support))::(fresh_variable_list2 (ar-1) support)

(******** Generation of recipe ********)

let recipe_of_variable var = Var(var)

let recipe_of_axiom axiom = Axiom(axiom)

let axiom_of_recipe = function
  | Axiom i -> Some i
  | _ -> None

let variable_of_recipe = function
  | Var v -> v
  | _ -> Debug.internal_error "[recipe.ml >> variable_of_recipe] A variable was expected"

let rec get_variables_of_recipe = function
  | Var v -> [v]
  | Axiom _ -> []
  | Func (_, list) -> List.concat (List.map get_variables_of_recipe list)

let rec is_closed = function
  | Var v -> false
  | Axiom _ -> true
  | Func (_, list) -> List.for_all is_closed list

let apply_function symbol list_sons = 
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if (List.length list_sons) <> Term.get_arity symbol
    then Debug.internal_error (Printf.sprintf "[recipe.ml >> apply_function] The function %s has arity %d but is given %d recipe" (Term.display_symbol_without_arity symbol) (Term.get_arity symbol) (List.length list_sons))
  );
  (***[END DEBUG]***)

  Func(symbol,list_sons)  
  
(******** Access functions *********)  

let top = function
  | Func(f,_) -> f
  | _ -> Debug.internal_error "[recipe.ml >> top] The recipe should not be a name nor a variable"

let get_support v = v.support
    
(******** Test functions *********)

let rec occurs var = function
  | Var(v) when v == var -> true
  | Var{link = RLink t;_} -> occurs var t
  | Func(_,args) -> List.exists (occurs var) args
  | _ -> false
  
let rec ax_occurs ax = function
  | Axiom(a) when a == ax -> true
  | Func(_,args) -> List.exists (ax_occurs ax) args
  | _ -> false

let is_equal_variable v1 v2 = v1 == v2  

let is_equal_axiom ax1 ax2 = ax1 = ax2  
  
let rec is_equal_recipe r1 r2 = match r1,r2 with
  | Var(v1),Var(v2) when v1 == v2 -> true
  | Axiom(n1),Axiom(n2) when n1 = n2 -> true
  | Func(f1,args1), Func(f2,args2) when Term.is_equal_symbol f1 f2 -> List.for_all2 is_equal_recipe args1 args2
  | _,_ -> false

let is_free_variable var = var.free

let is_free_variable2 = function
  | Var(v) -> v.free
  | _ -> false
  
let is_variable = function
  | Var(_) -> true
  | _ -> false
  
let is_axiom = function
  | Axiom(_) -> true
  | _ -> false
  
let is_function = function
  | Func(_,_) -> true
  | _ -> false

(******* Iterators ********)

let iter_args f_apply = function
  | Func(_,args) -> List.iter f_apply args
  | _ -> Debug.internal_error "[recipe.ml >> iter_args] The recipe should be an axiom nor a variable"
  
let map_args f_apply = function
  | Func(_,args) -> List.map f_apply args
  | _ -> Debug.internal_error "[recipe.ml >> map_args] The recipe should not be an axiom nor a variable"
  
(******* Substitution ********)

type substitution = (variable * recipe) list

let is_identity subst = subst = []

let create_substitution var recipe = [var,recipe]

let create_substitution2 var recipe = match var with
  | Var(v) -> [v,recipe]
  | _ -> Debug.internal_error "[recipe.ml >> create_substitution2] The recipe must be a variable"

let rec apply_substitution_on_recipe recipe = match recipe with
  | Func(f,args) -> Func(f, List.map apply_substitution_on_recipe args)
  | Var(v) -> 
      begin match v.link with
        | NoLink -> recipe
        | RLink r' -> r'
        | _ -> Debug.internal_error "[recipe.ml >> apply_substitution_on_recipe] Unexpected link"
      end
  | _ -> recipe
  
let apply_substitution subst elt f_iter_elt =
  (* Link the variables of the substitution *)
  List.iter (fun (v,t) -> v.link <- (RLink t)) subst;
  
  (* Apply the substitution on the element *)
  let new_elt = f_iter_elt elt apply_substitution_on_recipe in
  
  (* Unlink the variables of the substitution *)
  List.iter (fun (v,_) -> v.link <- NoLink) subst;
  
  new_elt
  
let equations_from_substitution = List.map (fun (v,r) -> Var(v),r)  

let filter_domain f_test subst = List.filter (fun (v,_) -> f_test v) subst

(****** Variable manipulation *******)

let linked_variables = ref []

let link var term = 
  var.link <- (RLink term);
  linked_variables := var::(!linked_variables)
  
let rec follow_link = function
  | Func(f,args) -> Func(f,List.map follow_link args)
  | Var({link = RLink t;_}) -> follow_link t
  | term -> term
  
let cleanup () =
  List.iter (fun var -> var.link <- NoLink) !linked_variables;
  linked_variables := []
  
(******* Syntactic unification *******)

exception Not_unifiable
  
let rec unify_term t1 t2 = match t1,t2 with
  | Var(v1), Var(v2) when v1 == v2 -> ()
  | Var({link = RLink t ; _}), _ -> unify_term t t2
  | _, Var({link = RLink t; _}) -> unify_term t1 t
  | Var(v1),Var(v2) ->
      if not v1.free
      then link v1 t2
      else link v2 t1
  | Var(v1), _ -> if occurs v1 t2 then raise Not_unifiable else link v1 t2
  | _, Var(v2) -> if occurs v2 t1 then raise Not_unifiable else link v2 t1
  | Axiom(n1), Axiom(n2) when n1 == n2 -> ()
  | Func(f1,args1), Func(f2,args2) ->  
      if f1 == f2 then List.iter2 unify_term args1 args2 else raise Not_unifiable
  | _,_ -> raise Not_unifiable
  
let unify eq_list = 
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if !linked_variables <> []
    then Debug.internal_error "[recipe.ml >> unify] The list of linked variables should be empty"
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


(***************************
***          Path        ***
****************************)

let rec path_of_recipe recipe = match recipe with
  | Axiom(i) -> ([],i)
  | Var(_) -> Debug.internal_error "[recipe.ml >> path_of_recipe] A path of the recipe should be closed"
  | Func(f,args) when Term.is_destructor f -> 
      let (l,r) = path_of_recipe (List.hd args) in
      (f::l,r)
  | Func(_,_) -> Debug.internal_error "[recipe.ml >> path_of_recipe] A path of a recipe should only contain destructor symbols and axiom" 
  
let apply_function_to_path symb (l_symb,ax) = (symb::l_symb,ax)

let axiom_path supp = ([],supp)
  
let is_equal_path (p1,ax1) (p2,ax2) = 
  if ax1 = ax2
  then
    let rec check_symbol l1 l2 = match l1,l2 with
      | [],[] -> true
      | [],_ | _,[] -> false
      | f1::q1,f2::q2 when Term.is_equal_symbol f1 f2 -> check_symbol q1 q2
      | _,_ -> false
    in
    
    check_symbol p1 p2
  else false
  
let rec is_recipe_same_path r1 r2 = match r1,r2 with
  | Axiom(i1), Axiom(i2) when i1 = i2 -> true
  | Func(f1,args1), Func(f2,args2) when (Term.is_equal_symbol f1 f2) && (Term.is_destructor f1) ->
      is_recipe_same_path (List.hd args1) (List.hd args2)
  | _,_ -> false
  
let rec is_path_of_recipe recipe (l,ax) = match l,recipe with
  | [],Axiom(i) when i = ax -> true
  | [],_ -> false
  | f1::q,Func(f2,args) when Term.is_equal_symbol f1 f2 ->
      is_path_of_recipe (List.hd args) (q,ax)
  | _,_ -> false
    
(*******************************
***          Context         ***
********************************)

let top_context = function
  | CFunc(f,_) -> f
  | _ -> Debug.internal_error "[recipe.ml >> top_context] The context should not be a path nor a variable"
  
let path_of_context = function
  | CPath p -> p
  | _ -> Debug.internal_error "[recipe.ml >> path_of_context] The context should be a path"

let rec context_of_recipe = function
  | Var(v) -> CVar(v)
  | Func(f,args) when Term.is_constructor f -> CFunc(f,List.map context_of_recipe args)
  | r -> CPath(path_of_recipe r) 
  
let rec recipe_of_context = function
  | CVar(v) -> Var(v)
  | CFunc(f,args) -> Func(f,List.map recipe_of_context args)
  | CPath _ -> Debug.internal_error "[recipe.ml >> recipe_of_context] The context should not contain any path"

(* Access *)

let rec get_max_param_context = function
  | CVar(v) -> v.support
  | CFunc(_,args) -> List.fold_left (fun acc pr -> max acc (get_max_param_context pr)) 0 args
  | CPath (_,i) -> i 
  
(* Testing *)

let rec is_closed_context = function
  | CVar(_) -> false
  | CPath(_) -> true
  | CFunc(_,args) -> List.for_all is_closed_context args  
  
let is_variable_context = function
  | CVar _ -> true
  | _ -> false
  
let is_path_context = function
  | CPath(_) -> true
  | _ -> false
  
let rec exists_path_in_context = function
  | CVar _ -> false
  | CPath _ -> true
  | CFunc(_,args) -> List.exists exists_path_in_context args

(** Substitution *)
  
let rec apply_substitution_on_pr = function
  | CFunc(f,args) -> CFunc(f, List.map apply_substitution_on_pr args)
  | CVar(v) -> 
      begin match v.link with
        | NoLink -> CVar(v)
        | CLink r' -> r'
        | _ -> Debug.internal_error "[recipe.ml >> apply_substitution_on_pr] Unexpected link"
      end
  | pr -> pr
  
let apply_substitution_on_context subst elt f_iter_elt =
  (* Link the variables of the substitution *)
  List.iter (fun (v,t) -> v.link <- (CLink (context_of_recipe t))) subst;
  
  (* Apply the substitution on the element *)
  let new_elt = f_iter_elt elt apply_substitution_on_pr in
  
  (* Unlink the variables of the substitution *)
  List.iter (fun (v,_) -> v.link <- NoLink) subst;
  
  new_elt
  
(*******************************
***         Formulas         ***
********************************)

exception Removal_transformation

type formula = (context * context) list

let create_formula var recipe = [CVar(var),context_of_recipe recipe]

let for_all_formula = List.for_all 

let exists_formula = List.exists  

let rec find_and_apply_formula f_test f_apply f_no_result = function
  | [] -> f_no_result ()
  | (t1,t2)::q -> 
      let r_test = f_test t1 t2 in
      if r_test
      then f_apply t1 t2
      else find_and_apply_formula f_test f_apply f_no_result q

(****** Simplification *******)      
      
let rec semi_unify all_path ref_list pr1 pr2 = match pr1,pr2 with
  | CVar(v1),CVar(v2) when v1 == v2 -> ()
  | CVar(_),CVar(_) | CVar(_), CFunc(_) -> 
      all_path := false;
      ref_list := (pr1,pr2)::!ref_list
  | CFunc(_),CVar(_) -> 
      all_path := false;
      ref_list := (pr2,pr1)::!ref_list
  | CVar(_),CPath(_) -> ref_list := (pr1,pr2)::!ref_list
  | CPath(_),CVar(_) -> ref_list := (pr2,pr1)::!ref_list
  | CPath(p1),CPath(p2) when is_equal_path p1 p2 -> ()
  | CFunc(f1,args1),CFunc(f2,args2) -> 
      if not (Term.is_equal_symbol f1 f2)
      then  Debug.internal_error "[recipe.ml >> semi_unify] The inequation disjunction should never be true";
      
      List.iter2 (semi_unify all_path ref_list) args1 args2
  | _,_ -> ref_list := (pr1,pr2)::!ref_list
  
let simplify_formula list_neq = 
  let ref_list = ref [] in
  let all_path = ref true in
  List.iter (fun (pr1,pr2) -> semi_unify all_path ref_list pr1 pr2) list_neq;
  if !all_path
  then raise Removal_transformation
  else !ref_list
  
(******* Substitution *******)  
  
let rec apply_substitution_on_context2 (v,pt) = function
  | CVar(v') when v == v' -> pt
  | CFunc(f,args) -> CFunc(f,List.map (apply_substitution_on_context2 (v,pt)) args)
  | pr -> pr
  
let rec apply_substitution_on_context_change_detected ref_change (v,pt) = function
  | CVar(v') when v == v' -> ref_change := true; pt
  | CFunc(f,args) -> CFunc(f,List.map (apply_substitution_on_context_change_detected ref_change (v,pt)) args)
  | pr -> pr
  
let apply_substitution_on_formulas subst elt f_iter_elt = match subst with
  | [v,recipe] -> let pt = context_of_recipe recipe in
      f_iter_elt elt (fun list_neq -> 
        List.map (fun (pt1,pt2) -> 
          apply_substitution_on_context2 (v,pt) pt1,
          apply_substitution_on_context2 (v,pt) pt2
        ) list_neq
      )
  | _ -> Debug.internal_error "[recipe.ml >> apply_substitution_on_formulas] The domain of the subtitution should contain only one variable"

let apply_simplify_substitution_on_formulas subst elt f_iter_elt = match subst with
  | [v,recipe] -> let pt = context_of_recipe recipe in
      f_iter_elt elt (fun list_neq -> 
        let was_modified = ref false in
        
        let formula' = 
          List.map (fun (pt1,pt2) -> 
            apply_substitution_on_context_change_detected was_modified (v,pt) pt1,
            apply_substitution_on_context_change_detected was_modified (v,pt) pt2
          ) list_neq
        in
          
        if !was_modified
        then simplify_formula formula'
        else list_neq
      )
  | _ -> Debug.internal_error "[recipe.ml >> apply_substitution_on_formulas] The domain of the subtitution should contain only one variable"
  
(*******************************
***         Display          ***
********************************)

let display_variable v = 
  if v.free
  then Printf.sprintf "%s^F_%d" v.id v.number
  else Printf.sprintf "%s^B_%d" v.id v.number
  
let display_axiom ax = Printf.sprintf "ax_%d" ax

let rec display_list_recipe f_apply = function
  | [] -> ""
  | [t] -> f_apply t
  | t::q -> Printf.sprintf "%s,%s" (f_apply t) (display_list_recipe f_apply q)

let rec display_recipe = function
  | Var(v) -> display_variable v
  | Axiom(ax) -> display_axiom ax
  | Func(f,args) -> 
      Printf.sprintf "%s(%s)" (Term.display_symbol_without_arity f) (display_list_recipe display_recipe args)     
      
and display_recipe2 assoc f_apply = function
  | Var(v) -> 
      begin try
        let _,t = List.find (fun (r,_) -> is_equal_recipe r (Var v)) assoc in
        f_apply t
      with
        Not_found -> display_variable v
      end
  | Axiom(ax) -> 
      begin try
        let _,t = List.find (fun (r,_) -> is_equal_recipe r (Axiom ax)) assoc in
        f_apply t
      with
        Not_found -> display_axiom ax
      end
  | Func(f,args) -> 
      Printf.sprintf "%s(%s)" (Term.display_symbol_without_arity f) (display_list_recipe (display_recipe2 assoc f_apply) args)
      
let display_path (l,ax) = 
  List.fold_right (fun f str->
    (Term.display_symbol_without_arity f)^":"^str
  ) l (display_axiom ax)
  
let rec display_context = function
  | CVar(v) -> display_variable v
  | CPath(p) -> display_path p
  | CFunc(f,args) -> Printf.sprintf "%s(%s)" (Term.display_symbol_without_arity f) (display_list_recipe display_context args) 
  
let rec display_formula = function
  | [] -> "BotF"
  | [c1,c2] -> (display_context c1)^" <> "^(display_context c2)
  | (c1,c2)::q -> (display_context c1)^" <> "^(display_context c2)^" \\/ "^(display_formula q)
  
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
  
  let add v = IntMap.add v.number
  
  let find v = IntMap.find v.number
  
  let mem v = IntMap.mem v.number
end
