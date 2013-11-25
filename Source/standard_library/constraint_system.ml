(*************************************************************************
** APTE v0.3.2beta - Algorithm for Proving Trace Equivalence            **
**                                                                      **
** Copyright (C) 2013  Vincent Cheval                                   **
**                                                                      **
** This program is free software: you can redistribute it and/or modify **
** it under the terms of the GNU General Public License as published by **
** the Free Software Foundation, either version 3 of the License, or    **
** any later version.                                                   **
**                                                                      **
** This program is distributed in the hope that it will be useful,      **
** but WITHOUT ANY WARRANTY; without even the implied warranty of       **
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 **
** See the GNU General Public License for more details.                 **
**                                                                      **
** You should have received a copy of the GNU General Public License    **
** along with this program.  If not, see http://www.gnu.org/licenses/   **
**************************************************************************)

(**********************************
***     Constraint system       ***
***********************************)

type message_inequation =
  {
    message_neq : Term.formula;
    assoc_table : Recipe.formula option
  }

type sub_csys = 
  {
    constraints_set : Constraint.Deducibility.elt Constraint.support_set;
    frame : Constraint.Frame.elt Constraint.support_set;
    
    maximum_support : int;
    
    conjunction_message_eq : (Term.term * Term.term) list;
    conjunction_recipe_eq : (Recipe.recipe * Recipe.recipe) list;
    
    (* Conjunction of inequation with possible universal variables *)
    conjunction_message_neq : message_inequation list;

    (* A list of constraints of the form: X_1 ... X_n |DEP| ax1 ... axn meaning that
    at least one Xi should depends on at least one axj *)
    dependency_constraints : (Recipe.recipe list * Recipe.axiom list) list;

    (** Help for the strategy *)
    semi_normal_form : bool;
    no_more_universal_var : bool;
    map_term_to_recipe : Recipe.variable Term.VariableMap.map; (* todo: recherche var et recette ds syst de contraintes *)
    map_recipe_to_term : Term.variable Recipe.VariableMap.map
  }
  
type constraint_system =
  | Bot
  | Csys of sub_csys

let empty = 
  Csys(
    {
      constraints_set = Constraint.empty_set;
      frame = Constraint.empty_set;
      maximum_support = 0;
      conjunction_message_eq = [];
      conjunction_recipe_eq = [];
      conjunction_message_neq = [];
      dependency_constraints = [];
      semi_normal_form = false;
      no_more_universal_var = false;
      map_term_to_recipe = Term.VariableMap.empty;
      map_recipe_to_term = Recipe.VariableMap.empty
    }
  )
  
let bottom = Bot

(******** Display ************)  

let rec display_conjunction f_apply = function
  | [] -> ""
  | [neq] -> Printf.sprintf "%s" (f_apply neq)
  | neq::q -> Printf.sprintf "%s /\\ %s" (f_apply neq) (display_conjunction f_apply q)  
  
let display_assoc_table = function
  | None -> "Bot"
  | Some(r_formula) -> Recipe.display_formula r_formula
    
let rec display_dependency_cst = function
  | [] -> Printf.sprintf "]\n"
  | (recipes, axioms) :: l -> String.concat "" (
    [Printf.sprintf "(" ] @
    List.map (fun r -> Printf.sprintf "%s," (Recipe.display_recipe r)) recipes @
    [Printf.sprintf " D- "] @
    List.map (fun r -> Printf.sprintf "%s," (Recipe.display_axiom r)) axioms @
    [Printf.sprintf ")"] @
    [display_dependency_cst l])

let display = function
  | Bot -> "Bot"
  | Csys(sub_csys) ->
      let line1 = "Csys(\n" in
      let frame = Printf.sprintf "frame = %s" (Constraint.display_vertically Constraint.Frame.display "   " sub_csys.frame) in
      let cons_set = Printf.sprintf "Deducibility constraints = %s" (Constraint.display_vertically Constraint.Deducibility.display "   " sub_csys.constraints_set) in
      let conj_mess_eq = Printf.sprintf "Message_equation = %s" 
        (display_conjunction (fun (t1,t2) -> Printf.sprintf "%s = %s" (Term.display_term t1) (Term.display_term t2)) 
        sub_csys.conjunction_message_eq) in
      (* let conj_recipe_eq = Printf.sprintf "Recipe_equation = %s"  *)
      (*   (display_conjunction (fun (r1,r2) -> Printf.sprintf "%s = %s" (Recipe.display_recipe r1) (Recipe.display_recipe r2))  *)
      (*   sub_csys.conjunction_recipe_eq) in *)      
      let dep_cst = Printf.sprintf "Dependency_constraints = [%s" (display_dependency_cst sub_csys.dependency_constraints) in
      let formula = Printf.sprintf "Formula = %s" (display_conjunction (fun neq -> (Term.display_formula neq.message_neq)^"("^(display_assoc_table neq.assoc_table)^")") sub_csys.conjunction_message_neq) in
      (* Do More *)
      let endline = ")\n" in
      
      Printf.sprintf "%s%s\n%s\n%s\n%s\n%s\n%s\n" line1 frame cons_set dep_cst formula conj_mess_eq endline

let display_dependency_constraints = function
  | Bot -> "Dependency constraints : [] (Bot)."
  | Csys(sub_csys) -> Printf.sprintf "Dependency constraints = [%s"
    (display_dependency_cst sub_csys.dependency_constraints)

(******** Addition functions ********)

let add_message_equation csys t1 t2 = match csys with
  | Bot -> Debug.internal_error "[constraint_system.ml >> add_message_equation] An equation cannot be added to a bottom constraint system"
  | Csys(sub_csys) ->
      Csys({ sub_csys with conjunction_message_eq = (t1,t2)::sub_csys.conjunction_message_eq })
      
let add_message_formula csys formula = match csys with
  | Bot -> Debug.internal_error "[constraint_system.ml >> add_message_formula] An equation cannot be added to a bottom constraint system"
  | Csys(sub_csys) ->
      Csys({ sub_csys with conjunction_message_neq = { message_neq = formula ; assoc_table = None}::sub_csys.conjunction_message_neq })
      
let add_new_deducibility_constraint csys var_r term = match csys with
  | Bot -> Debug.internal_error "[constraint_system.ml >> add_new_deduciblity_constraint] A deduciblity constraint cannot be added to a bottom constraint system"
  | Csys(sub_csys) ->
      (***[BEGIN DEBUG]***)
      Debug.high_debugging (fun () ->
        if not (Term.is_constructor_term term)
        then Debug.internal_error "[constraint_system.ml >> add_new_deduciblity_constraint] A deduciblity constraint cannot be added without a constructor term as right hand term."
      );
      
      Debug.low_debugging (fun () ->
        if sub_csys.maximum_support <> Recipe.get_support var_r
        then Debug.internal_error "[constraint_system.ml >> add_new_deduciblity_constraint] The support of the recipe variable should have be the maximal support of the constraint system."
      );
      (***[END DEBUG]***)
      
      let cons = Constraint.Deducibility.create var_r (Recipe.get_support var_r) term in
      let new_constraints_set = Constraint.add Constraint.Deducibility.get_support cons sub_csys.constraints_set in
      Csys({ sub_csys with constraints_set = new_constraints_set })

let add_new_dependency_constraint csys recipes axioms = match csys with
  | Bot -> Debug.internal_error "[constraint_system.ml >> add_new_dependency_constraint] A dependency constraint cannot be added to a bottom constraint system"
  | Csys(sub_csys) ->
    let new_dep_constraints = (recipes, axioms) :: sub_csys.dependency_constraints in
      Csys({ sub_csys with dependency_constraints = new_dep_constraints })

let add_new_axiom csys term = match csys with
  | Bot -> Debug.internal_error "[constraint_system.ml >> add_axiom] An axiom cannot be added to a bottom constraint system"
  | Csys(sub_csys) ->
      let new_frame = Constraint.add_new_support (fun s -> 
        Constraint.Frame.create (Recipe.recipe_of_axiom (Recipe.axiom s)) s term
      ) sub_csys.frame in
      Csys({ sub_csys with frame = new_frame ; maximum_support = sub_csys.maximum_support + 1 })
      
let add_frame_constraint csys fc_list = match csys with
  | Bot -> Debug.internal_error "[constraint_system.ml >> add_frame_constraint] A frame constraint cannot be added to a bottom constraint system"
  | Csys(sub_csys) ->   
      (***[BEGIN DEBUG]***)
      Debug.high_debugging (fun () ->
        if List.exists (fun fc -> not (Term.is_constructor_term (Constraint.Frame.get_message fc))) fc_list
        then Debug.internal_error "[constraint_system.ml >> add_frame_constraint] A frame constraint cannot be added without a constructor term as right hand term."
      );
      (***[END DEBUG]***)
  
      let new_frame = Constraint.add_list Constraint.Frame.get_support fc_list sub_csys.frame in
      Csys({ sub_csys with frame = new_frame})
      
let add_deducibility_constraint csys dc_list = match csys with
  | Bot -> Debug.internal_error "[constraint_system.ml >> add_deducibility_constraint] A deducibility constraint cannot be added to a bottom constraint system"
  | Csys(sub_csys) ->   
      (***[BEGIN DEBUG]***)
      Debug.high_debugging (fun () ->
        if List.exists (fun dc -> not (Term.is_constructor_term (Constraint.Deducibility.get_message dc))) dc_list
        then Debug.internal_error "[constraint_system.ml >> add_deducibility_constraint] A deducibility constraint cannot be added without a constructor term as right hand term."
      );
      (***[END DEBUG]***)
  
      let new_cons_set = Constraint.add_list Constraint.Deducibility.get_support dc_list sub_csys.constraints_set in
      Csys({ sub_csys with constraints_set = new_cons_set})
      
(******** Access *********)

let get_deducibility_constraint_set = function
  | Bot -> Debug.internal_error "[Constraint_system.ml >> get_deducibility_constraints_set] The constraint system is bottom"
  | Csys(sub_csys) -> sub_csys.constraints_set

let get_frame = function
  | Bot -> Debug.internal_error "[Constraint_system.ml >> get_frame] The constraint system is bottom"
  | Csys(sub_csys) -> sub_csys.frame

let get_message_equations = function
  | Bot -> Debug.internal_error "[Constraint_system.ml >> get_message_equations] The constraint system is bottom"
  | Csys(sub_csys) -> sub_csys.conjunction_message_eq

let get_recipe_equations = function
  | Bot -> Debug.internal_error "[Constraint_system.ml >> get_recipe_equations] The constraint system is bottom"
  | Csys(sub_csys) -> sub_csys.conjunction_recipe_eq

let get_dependency_constraints = function
  | Bot -> Debug.internal_error "[Constraint_system.ml >> get_dependency_constraints] The constraint system is bottom"
  | Csys(sub_csys) -> sub_csys.dependency_constraints
  
let get_maximal_support = function 
  | Bot -> Debug.internal_error "[Constraint_system.ml >> get_maximal_support] The constraint system is bottom"
  | Csys(sub_csys) -> sub_csys.maximum_support
  
(******** Modifications functions ********)

let frame_replace csys pos f_replace = match csys with
  | Bot -> Debug.internal_error "[Constraint_system.ml >> frame_replace] The constraint system is bottom"
  | Csys(sub_csys) ->
      let (elt,frame) = Constraint.replace pos f_replace sub_csys.frame in
  
      elt,Csys({sub_csys with frame = frame})

let frame_replace2 csys pos f_replace = match csys with
  | Bot -> Debug.internal_error "[Constraint_system.ml >> frame_replace2] The constraint system is bottom"
  | Csys(sub_csys) ->
      let (elt,frame1,frame2) = Constraint.replace2 pos f_replace sub_csys.frame in
  
      elt,Csys({sub_csys with frame = frame1}),Csys({sub_csys with frame = frame2})
  
let frame_search_and_replace csys range f_test f_replace =match csys with
  | Bot -> Debug.internal_error "[Constraint_system.ml >> frame_search_and_replace] The constraint system is bottom"
  | Csys(sub_csys) ->
      let (elt,pos,frame) = Constraint.search_and_replace range f_test f_replace sub_csys.frame in
  
      elt,pos,Csys({sub_csys with frame = frame})

let frame_search_and_replace2 csys range f_test f_replace = match csys with
  | Bot -> Debug.internal_error "[Constraint_system.ml >> frame_search_and_replace2] The constraint system is bottom"
  | Csys(sub_csys) ->
      let (elt,pos,frame1,frame2) = Constraint.search_and_replace2 range f_test f_replace sub_csys.frame in
  
      elt,pos,Csys({sub_csys with frame = frame1}),Csys({sub_csys with frame = frame2})
 
let set_semi_solved_form csys = match csys with
  | Bot -> Bot
  | Csys(sub_csys) -> 
      Csys({ sub_csys with semi_normal_form = true})   
      
let unset_semi_solved_form csys = match csys with
  | Bot -> Bot
  | Csys(sub_csys) -> 
      Csys({ sub_csys with semi_normal_form = false})   
      
let set_no_universal_variable csys = match csys with
  | Bot -> Bot
  | Csys(sub_csys) -> 
      Csys({ sub_csys with no_more_universal_var = true})   
      
let unset_no_universal_variable csys = match csys with
  | Bot -> Bot
  | Csys(sub_csys) -> 
      Csys({ sub_csys with no_more_universal_var = false})   
      
(********* Iterators *********)

exception Bottom_constraint_system 

let map_message_inequation f = function
  | Bot -> Bot
  | Csys(sub_csys) ->
      try
        let conj_neq' = 
          List.fold_right (fun neq acc ->
            let formula = f neq.message_neq in
            if Term.is_bottom formula
            then raise Bottom_constraint_system
            else if Term.is_top formula
            then acc
            else {neq with message_neq = formula}::acc
          ) sub_csys.conjunction_message_neq []
       
        in
        
        Csys({ sub_csys with conjunction_message_neq = conj_neq'})
      with 
        Bottom_constraint_system -> Bot

let map_message sub_csys f_apply = 
  let cons_set = Constraint.map Constraint.SAll (fun dc -> Constraint.Deducibility.replace_message dc f_apply) sub_csys.constraints_set in
  let frame = Constraint.map Constraint.SAll (fun fc -> Constraint.Frame.replace_message fc f_apply) sub_csys.frame in
  
  let conj_mess_neq = 
    List.map (fun neq ->
      { neq with message_neq = Term.map_term_formula neq.message_neq f_apply }
    ) sub_csys.conjunction_message_neq 
  in
    
  { sub_csys with
    constraints_set = cons_set;
    frame = frame;
    conjunction_message_neq = conj_mess_neq
  }
  
let map_message_simplify simplify_func sub_csys f_apply =
  let cons_set = Constraint.map Constraint.SAll (fun dc -> Constraint.Deducibility.replace_message dc (fun t -> let _,t' = f_apply t in t')) sub_csys.constraints_set in
      
  let frame = Constraint.map Constraint.SAll (fun fc -> Constraint.Frame.replace_message fc (fun t -> let _,t' = f_apply t in t')) sub_csys.frame in
  
  let rec conj_mess_neq = function
    | [] -> []
    | neq::q ->
        let change_detected,formula = Term.map_term_formula_change_detected neq.message_neq f_apply in
        if change_detected
        then 
          let formula' = simplify_func formula in
          if Term.is_bottom formula'
          then raise Bottom_constraint_system
          else if Term.is_top formula'
          then conj_mess_neq q
          else {neq with message_neq = formula'}::(conj_mess_neq q) 
        else neq::(conj_mess_neq q)
  in
  
  { sub_csys with
    constraints_set = cons_set;
    frame = frame;
    conjunction_message_neq = conj_mess_neq sub_csys.conjunction_message_neq
  }  
  

  
(******** Testing functions *********)

let is_semi_solved_form csys = match csys with
  | Bot -> true
  | Csys(sub_csys) -> sub_csys.semi_normal_form
  
let is_no_universal_variable csys = match csys with
  | Bot -> true
  | Csys(sub_csys) -> sub_csys.no_more_universal_var

let is_bottom = function
  | Bot -> true
  | _ -> false
  
let check_same_structure csys1 csys2 = match csys1,csys2 with
  | Bot, _ -> ()
  | _, Bot -> ()
  | Csys(sub_csys1), Csys(sub_csys2) -> 
      if not (Constraint.Deducibility.is_same_structure sub_csys1.constraints_set sub_csys2.constraints_set)
      then Debug.internal_error "[constraint_system.ml >> check_same_structure] The two constraint systems do not have the same structure on the deducbility constraint set.";
  
      if not (Constraint.Frame.is_same_structure sub_csys1.frame sub_csys2.frame)
      then Debug.internal_error "[constraint_system.ml >> check_structure] The two constraint systems do not have the same structure on the frame.";
    
      if sub_csys1.maximum_support <> sub_csys2.maximum_support
      then Debug.internal_error "[constraint_system.ml >> check_same_structure] The two constraint systems do not have same maximum support";
  
      if not (List.for_all2 (fun (r11,r12) (r21,r22) -> Recipe.is_equal_recipe r11 r21 && Recipe.is_equal_recipe r12 r22) sub_csys1.conjunction_recipe_eq sub_csys2.conjunction_recipe_eq)
      then Debug.internal_error "[constraint_system.ml >> check_same_structure] The two constraint systems do not have same recipe equations"
  
let check_same_shape csys1 csys2 = match csys1,csys2 with
  | Bot, _ -> ()
  | _, Bot -> ()
  | Csys(sub_csys_1), Csys(sub_csys_2) -> 
      let set_s2_sub_csys1 = ref []
      and set_s2_sub_csys2 = ref [] in
  
      Constraint.iter Constraint.SAll (fun cons ->
        let var = Constraint.Deducibility.get_recipe_variable cons in
        if Recipe.is_free_variable var
        then set_s2_sub_csys1 := var::!set_s2_sub_csys1
      ) sub_csys_1.constraints_set;
  
      Constraint.iter Constraint.SAll (fun cons ->
        let var = Constraint.Deducibility.get_recipe_variable cons in
        if Recipe.is_free_variable var
        then set_s2_sub_csys2 := var::!set_s2_sub_csys2
      ) sub_csys_2.constraints_set;
  
      let rec get_var ref_l recipe = 
        if Recipe.is_variable recipe 
        then 
          if not (Recipe.is_free_variable2 recipe) || List.exists (Recipe.is_equal_variable (Recipe.variable_of_recipe recipe)) !ref_l
          then ()
          else ref_l := (Recipe.variable_of_recipe recipe)::!ref_l
        else if Recipe.is_axiom recipe
        then ()
        else Recipe.iter_args (get_var ref_l) recipe
      in
    
      List.iter (fun (r1,r2) -> get_var set_s2_sub_csys1 r1; get_var set_s2_sub_csys1 r2) sub_csys_1.conjunction_recipe_eq;
      List.iter (fun (r1,r2) -> get_var set_s2_sub_csys2 r1; get_var set_s2_sub_csys2 r2) sub_csys_2.conjunction_recipe_eq;
  
      if not (List.for_all (fun r1 -> List.exists (Recipe.is_equal_variable r1) !set_s2_sub_csys1) !set_s2_sub_csys2)
        || not (List.for_all (fun r1 -> List.exists (Recipe.is_equal_variable r1) !set_s2_sub_csys2) !set_s2_sub_csys1)
      then Debug.internal_error "[constraint_system.ml >> check_same_shape] The two constraint systems do not have the same shape"
 
let is_unsatisfiable csys = match csys with
  | Bot -> true
  | Csys(sub_csys) -> 
      Constraint.exists Constraint.SAll (Constraint.Deducibility.is_unsatisfiable sub_csys.frame) sub_csys.constraints_set
      
(**************************************
***     Phase 1 functionnalities    ***
***************************************)  

module Phase_1 =
struct
  
  let activate_phase = function
    | Bot -> Bot
    | Csys(sub_csys) -> 
        Csys({ sub_csys with
          conjunction_message_neq = List.map (fun neq -> { neq with assoc_table = None } ) sub_csys.conjunction_message_neq;
          semi_normal_form = false;
          no_more_universal_var = false;
          map_term_to_recipe = Term.VariableMap.empty;
          map_recipe_to_term = Recipe.VariableMap.empty
        })
    
  (******** Modifications functions ********)

  let deducibility_replace csys pos f_replace = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_1.deducibility_replace] The constraint system is bottom"
    | Csys(sub_csys) ->
        let (elt,dc_set) = Constraint.replace pos f_replace sub_csys.constraints_set in
  
        elt,Csys({sub_csys with constraints_set = dc_set})

  let deducibility_replace2 csys pos f_replace = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_1.deducibility_replace2] The constraint system is bottom"
    | Csys(sub_csys) ->
        let (elt,dc_set1,dc_set2) = Constraint.replace2 pos f_replace sub_csys.constraints_set in
  
        elt,Csys({sub_csys with constraints_set = dc_set1}),Csys({sub_csys with constraints_set = dc_set2})
  
  let deducibility_search_and_replace csys range f_test f_replace = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_1.deducibility_search_and_replace] The constraint system is bottom"
    | Csys(sub_csys) ->
        let (elt,pos,dc_set) = Constraint.search_and_replace range f_test f_replace sub_csys.constraints_set in
  
        elt,pos,Csys({sub_csys with constraints_set = dc_set})

  let deducibility_search_and_replace2 csys range f_test f_replace = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_1.deducibility_search_and_replace2] The constraint system is bottom"
    | Csys(sub_csys) ->
        let (elt,pos,dc_set1,dc_set2) = Constraint.search_and_replace2 range f_test f_replace sub_csys.constraints_set in
  
        elt,pos,Csys({sub_csys with constraints_set = dc_set1}),Csys({sub_csys with constraints_set = dc_set2})     
    
  (******** Substitutions ********)
    
  let unify_and_apply_message_equations csys list_eq = match csys with
    | Bot -> Bot
    | Csys(sub_csys) ->
        begin try
          let sub_csys' = Term.unify_and_apply_change_detected list_eq sub_csys (map_message_simplify Term.simplify_formula) in
          Csys({ sub_csys' with conjunction_message_eq = list_eq @ sub_csys'.conjunction_message_eq })
        with Bottom_constraint_system -> Bot
        end
        
  let apply_message_substitution csys subst = match csys with
    | Bot -> Bot
    | Csys(sub_csys) -> 
        begin try
          let sub_csys' = Term.apply_substitution_change_detected subst sub_csys (map_message_simplify Term.simplify_formula) in
          let list_eq = Term.equations_of_substitution subst in
          Csys({ sub_csys' with conjunction_message_eq = list_eq @ sub_csys'.conjunction_message_eq })
        with Bottom_constraint_system -> Bot
        end

  let apply_recipe_substitution csys subst =
    
    let map_frame frame f_apply = 
       Constraint.map Constraint.SAll (fun fc -> Constraint.Frame.replace_recipe fc f_apply) frame
    in
  
    match csys with
      | Bot -> Bot
      | Csys(sub_csys) ->
          (***[BEGIN DEBUG]***)
          Debug.high_debugging (fun () ->
            let test_subst = 
              Recipe.filter_domain (fun v -> 
                Constraint.exists Constraint.SAll (fun dc -> 
                  Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable dc) v
                ) sub_csys.constraints_set
              ) subst
            in
           
            if not (Recipe.is_identity test_subst)
            then Debug.internal_error "[constraint_system.ml >> Phase_1.apply_recipe_substitution] The domain of the substitution shall never infer the deducibility constraints."
          );
          (***[END DEBUG]***)
    
          let equations = Recipe.equations_from_substitution subst in
          let rec apply_to_dep_csts = function
            | [] -> []
            | (lr, la) :: tl -> 
              ((Recipe.apply_substitution subst lr (fun l f -> List.map f l)), la)
                 :: (apply_to_dep_csts tl)  in

          Csys({ sub_csys with
            frame = Recipe.apply_substitution subst sub_csys.frame map_frame;
            dependency_constraints = apply_to_dep_csts sub_csys.dependency_constraints;
            conjunction_recipe_eq = equations@(sub_csys.conjunction_recipe_eq)
            })
            
  let normalise = function
    | Bot -> Bot
    | Csys(sub_csys) ->
        try
          let subst = Term.unify_modulo_rewrite_rules sub_csys.conjunction_message_eq in
          let sub_csys' = Term.apply_substitution subst sub_csys map_message in
      
          let rec simplify_neq = function
            | [] -> []
            | neq::q ->
                let formula' = Term.simplify_formula_modulo_rewrite_rules neq.message_neq in
                if Term.is_bottom formula'
                then raise Bottom_constraint_system
                else if Term.is_top formula'
                then simplify_neq q
                else {neq with message_neq = formula'}::(simplify_neq q)
          in
      
          try
            Csys({ sub_csys' with 
              conjunction_message_neq = simplify_neq sub_csys'.conjunction_message_neq;
              conjunction_message_eq = Term.equations_of_substitution (Term.filter_domain (fun v -> (Term.get_quantifier v) = Term.Free) subst)
            })
          with Bottom_constraint_system -> Bot
        with Term.Not_unifiable -> Bot
end
  
(**************************************
***     Phase 2 functionnalities    ***
***************************************)  
  
module Phase_2 =
struct
  
  let activate_phase = function
    | Bot -> Bot
    | Csys(sub_csys) -> 
        if sub_csys.semi_normal_form
        then Debug.internal_error "[Constraint_system.ml >> Phase_2.activate_phase] This should not be a semi normal form"
        else
        
          let map_t_to_r = Term.VariableMap.empty
          and map_r_to_t = Recipe.VariableMap.empty in
          
          let map_t_to_r', map_r_to_t' = 
            Constraint.fold_left Constraint.SAll (fun (t_to_r,r_to_t) dc ->
              let v_r = Constraint.Deducibility.get_recipe_variable dc
              and v_t = Term.variable_of_term (Constraint.Deducibility.get_message dc) in
              (Term.VariableMap.add v_t v_r t_to_r, Recipe.VariableMap.add v_r v_t r_to_t)
            ) (map_t_to_r,map_r_to_t) sub_csys.constraints_set
          in
        
          Csys({ sub_csys with
            conjunction_message_neq = List.map (fun neq -> { neq with assoc_table = None } ) sub_csys.conjunction_message_neq;
            semi_normal_form = true;
            no_more_universal_var = false;
            map_term_to_recipe = map_t_to_r';
            map_recipe_to_term = map_r_to_t'
          })
        
  let add_message_inequation csys m_var m_recipe var recipe = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_2.add_message_inequation] The constraint system is bottom"
    | Csys(sub_csys) -> 
        let message_neq = sub_csys.conjunction_message_neq in  
        let message_neq' = List.filter (fun neq -> not (Term.is_in_formula (Term.term_of_variable m_var) m_recipe neq.message_neq)) message_neq in
        let formula_r = Recipe.create_formula var recipe
        and formula = Term.create_inequation (Term.term_of_variable m_var) m_recipe in
    
    Csys({ sub_csys with  conjunction_message_neq = ({ message_neq = formula; assoc_table = Some(formula_r)})::message_neq' })

  
  (******** Modifications functions ********)

  let deducibility_replace csys pos f_replace = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_2.deducibility_replace] The constraint system is bottom"
    | Csys(sub_csys) ->
        let r_var_map = ref sub_csys.map_recipe_to_term
        and m_var_map = ref sub_csys.map_term_to_recipe in
    
        let (elt,dc_set) = 
          Constraint.replace pos (fun dc ->
            let dc_l = f_replace dc in
            List.iter (fun dc' -> 
              let r_var = Constraint.Deducibility.get_recipe_variable dc'
              and m_var = Term.variable_of_term (Constraint.Deducibility.get_message dc') in
              m_var_map := Term.VariableMap.add m_var r_var !m_var_map;
              r_var_map := Recipe.VariableMap.add r_var m_var !r_var_map
            ) dc_l;
            dc_l
          ) sub_csys.constraints_set in
        
        elt,Csys({sub_csys with constraints_set = dc_set; map_recipe_to_term = !r_var_map; map_term_to_recipe = !m_var_map})

  let deducibility_replace2 csys pos f_replace = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_2.deducibility_replace] The constraint system is bottom"
    | Csys(sub_csys) ->
        let r_var_map1 = ref sub_csys.map_recipe_to_term
        and m_var_map1 = ref sub_csys.map_term_to_recipe
        and r_var_map2 = ref sub_csys.map_recipe_to_term
        and m_var_map2 = ref sub_csys.map_term_to_recipe in
        
        let (elt,dc_set1,dc_set2) = 
          Constraint.replace2 pos (fun dc ->
            let dc_l1,dc_l2 = f_replace dc in
            
            List.iter (fun dc' -> 
              let r_var = Constraint.Deducibility.get_recipe_variable dc'
              and m_var = Term.variable_of_term (Constraint.Deducibility.get_message dc') in
              m_var_map1 := Term.VariableMap.add m_var r_var !m_var_map1;
              r_var_map1 := Recipe.VariableMap.add r_var m_var !r_var_map1
            ) dc_l1;
            
            List.iter (fun dc' -> 
              let r_var = Constraint.Deducibility.get_recipe_variable dc'
              and m_var = Term.variable_of_term (Constraint.Deducibility.get_message dc') in
              m_var_map2 := Term.VariableMap.add m_var r_var !m_var_map2;
              r_var_map2 := Recipe.VariableMap.add r_var m_var !r_var_map2
            ) dc_l2;
            
            dc_l1,dc_l2
          ) sub_csys.constraints_set in
  
        elt,
        Csys({sub_csys with constraints_set = dc_set1; map_recipe_to_term = !r_var_map1; map_term_to_recipe = !m_var_map1}),
        Csys({sub_csys with constraints_set = dc_set2; map_recipe_to_term = !r_var_map2; map_term_to_recipe = !m_var_map2})
  
  let deducibility_search_and_replace csys range f_test f_replace = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_2.deducibility_search_and_replace] The constraint system is bottom"
    | Csys(sub_csys) ->
        let r_var_map = ref sub_csys.map_recipe_to_term
        and m_var_map = ref sub_csys.map_term_to_recipe in
    
        let (elt,pos,dc_set) = 
          Constraint.search_and_replace range f_test (fun dc ->
            let dc_l = f_replace dc in
            
            List.iter (fun dc' -> 
              let r_var = Constraint.Deducibility.get_recipe_variable dc'
              and m_var = Term.variable_of_term (Constraint.Deducibility.get_message dc') in
              m_var_map := Term.VariableMap.add m_var r_var !m_var_map;
              r_var_map := Recipe.VariableMap.add r_var m_var !r_var_map
            ) dc_l;
            
            dc_l
          ) sub_csys.constraints_set in
  
        elt,pos,Csys({sub_csys with constraints_set = dc_set; map_recipe_to_term = !r_var_map; map_term_to_recipe = !m_var_map})

  let deducibility_search_and_replace2 csys range f_test f_replace = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> Phase_2.deducibility_search_and_replace2] The constraint system is bottom"
    | Csys(sub_csys) ->
        let r_var_map1 = ref sub_csys.map_recipe_to_term
        and m_var_map1 = ref sub_csys.map_term_to_recipe
        and r_var_map2 = ref sub_csys.map_recipe_to_term
        and m_var_map2 = ref sub_csys.map_term_to_recipe in
        
        
        let (elt,pos,dc_set1,dc_set2) = 
          Constraint.search_and_replace2 range f_test (fun dc ->
            let dc_l1,dc_l2 = f_replace dc in
            
            List.iter (fun dc' -> 
              let r_var = Constraint.Deducibility.get_recipe_variable dc'
              and m_var = Term.variable_of_term (Constraint.Deducibility.get_message dc') in
              m_var_map1 := Term.VariableMap.add m_var r_var !m_var_map1;
              r_var_map1 := Recipe.VariableMap.add r_var m_var !r_var_map1
            ) dc_l1;
            
            List.iter (fun dc' -> 
              let r_var = Constraint.Deducibility.get_recipe_variable dc'
              and m_var = Term.variable_of_term (Constraint.Deducibility.get_message dc') in
              m_var_map2 := Term.VariableMap.add m_var r_var !m_var_map2;
              r_var_map2 := Recipe.VariableMap.add r_var m_var !r_var_map2
            ) dc_l2;
            
            dc_l1,dc_l2
          ) sub_csys.constraints_set in
  
        elt,
        pos,
        Csys({sub_csys with constraints_set = dc_set1; map_recipe_to_term = !r_var_map1; map_term_to_recipe = !m_var_map1}),
        Csys({sub_csys with constraints_set = dc_set2; map_recipe_to_term = !r_var_map2; map_term_to_recipe = !m_var_map2})    
    
  (******** Substitutions ********)      
        
  let unify_and_apply_message_equations csys list_eq = match csys with
    | Bot -> Bot
    | Csys(sub_csys) ->
        begin try
          let sub_csys' = Term.unify_and_apply_change_detected list_eq sub_csys (map_message_simplify Term.simplify_formula_phase_2) in
          Csys({ sub_csys' with conjunction_message_eq = list_eq @ sub_csys'.conjunction_message_eq })
        with Bottom_constraint_system -> Bot
        end
        
  let apply_message_substitution csys subst = match csys with
    | Bot -> Bot
    | Csys(sub_csys) -> 
        begin try
          let sub_csys' = Term.apply_substitution_change_detected subst sub_csys (map_message_simplify Term.simplify_formula_phase_2) in
          let list_eq = Term.equations_of_substitution subst in
          Csys({ sub_csys' with conjunction_message_eq = list_eq @ sub_csys'.conjunction_message_eq })
        with Bottom_constraint_system -> Bot
        end
        
  let apply_recipe_substitution csys subst =
    
    let map_frame frame f_apply = 
       Constraint.map Constraint.SAll (fun fc -> Constraint.Frame.replace_recipe fc f_apply) frame
    in
    
    let map_neq_cunj neq_cunj f_apply = 
      List.map (fun neq ->
        { neq with 
          assoc_table = 
            begin match neq.assoc_table with
              | None -> None
              | Some(r_formula) -> 
                 begin try 
                   Some(f_apply r_formula)
                 with Recipe.Removal_transformation -> None
                 end
            end
        }
      )   neq_cunj
    in
  
    match csys with
      | Bot -> Bot
      | Csys(sub_csys) ->
          (***[BEGIN DEBUG]***)
          Debug.high_debugging (fun () ->
            let test_subst = 
              Recipe.filter_domain (fun v -> 
                Constraint.exists Constraint.SAll (fun dc -> 
                  Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable dc) v
                ) sub_csys.constraints_set
              ) subst
            in
           
            if not (Recipe.is_identity test_subst)
            then Debug.internal_error "[constraint_system.ml >> Phase_2.apply_recipe_substitution] The domain of the substitution shall never infer the deducibility constraints."
          );
          (***[END DEBUG]***)
    
          let equations = Recipe.equations_from_substitution subst in
          
          Csys({ sub_csys with
            frame = Recipe.apply_substitution subst sub_csys.frame map_frame;
            conjunction_message_neq = Recipe.apply_simplify_substitution_on_formulas subst sub_csys.conjunction_message_neq map_neq_cunj;
            conjunction_recipe_eq = equations@(sub_csys.conjunction_recipe_eq)
            })
    
  (********* Access **********)

  let term_of_recipe csys recipe = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> term_of_recipe] The constraint system is bottom"
    | Csys(sub_csys) -> 
  
        let rec sub_t_of_r recipe = 
          if Recipe.is_variable recipe
          then 
            let v = Recipe.VariableMap.find ((Recipe.variable_of_recipe recipe)) sub_csys.map_recipe_to_term in
            Term.term_of_variable v
          else if Recipe.is_function recipe && Term.is_constructor (Recipe.top recipe)
          then Term.apply_function (Recipe.top recipe) (Recipe.map_args sub_t_of_r recipe)
          else Debug.internal_error "[constraint_system.ml >> term_of_recipe] There should not be any axiom" 
        in
  
        sub_t_of_r recipe
   
  let recipe_of_term csys term = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> recipe_of_term] The constraint system is bottom"
    | Csys(sub_csys) -> 
        
        let rec sub_r_to_t term = 
          if Term.is_variable term
          then
            let var_r = Term.VariableMap.find (Term.variable_of_term term) sub_csys.map_term_to_recipe in
            Recipe.recipe_of_variable var_r
          else if Term.is_function term && Term.is_constructor (Term.top term)
          then Recipe.apply_function (Term.top term) (Term.map_args sub_r_to_t term)
          else Debug.internal_error "[constraint_system.ml >> recipe_of_term] There should not be any name"  
        in
  
        sub_r_to_t term
    
  let get_max_param_context csys recipe = match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> get_max_param_context] The constraint system is bottom"
    | Csys(_) -> Recipe.get_max_param_context (Recipe.context_of_recipe recipe)

  let get_max_param_context_from_term csys term =  match csys with
    | Bot -> Debug.internal_error "[Constraint_system.ml >> get_max_param_context_from_term] The constraint system is bottom"
    | Csys(sub_csys) ->
        let rec sub_param_max term =
          if Term.is_variable term 
          then 
            let var_r = Term.VariableMap.find (Term.variable_of_term term) sub_csys.map_term_to_recipe in
            Recipe.get_support var_r
          else if Term.is_function term && Term.is_constructor (Term.top term)
          then Term.fold_left_args (fun max_acc t -> max max_acc (sub_param_max t)) 0 term
          else Debug.internal_error "[constraint_system.ml >> get_param_max_context_from_term] There should not be any name"
        in
  
        sub_param_max term
        
  let map_message_inequations f_apply csys = match csys with
    | Bot -> Bot
    | Csys(sub_csys) ->
        try 
        Csys({ 
          sub_csys with conjunction_message_neq = 
            List.fold_right (fun neq acc ->
              let (formula',assoc') = f_apply neq.message_neq neq.assoc_table in
          
              if Term.is_top formula'
              then acc
              else if Term.is_bottom formula'
              then raise Bottom_constraint_system 
              else { message_neq = formula'; assoc_table = assoc' }::acc
            ) sub_csys.conjunction_message_neq []
        })
        with Bottom_constraint_system -> Bot
       
   let fold_left_message_inequation f_apply acc csys = match csys with
    | Bot -> acc
    | Csys(sub_csys) ->
        List.fold_left (fun acc' neq -> f_apply acc' neq.message_neq neq.assoc_table) acc sub_csys.conjunction_message_neq
        
end

(*******************************************
***    Row matrix of constraint system   ***
********************************************)

type row_matrix =
  {
    number_column : int;
    max_support : int;
    row : constraint_system array  
  }
  
module Row =
struct
  
  exception All_bottom

  let create nb_column csys_list = 
    (***[BEGIN DEBUG]***)
    Debug.high_debugging (fun () ->
      ignore (
        List.fold_left (fun csys_acc csys -> match csys_acc,csys with
          | Csys(_),Bot | Bot,Bot -> csys_acc
          | Bot,Csys(_) -> csys
          | _,_ -> 
              check_same_structure csys_acc csys;
              csys_acc
        ) Bot csys_list
      )
    );
    (***[END DEBUG]***)
  
    let vect = Array.make nb_column Bot in
  
    let nb_elt_l, supp = 
      List.fold_left (fun (i,supp) csys -> 
        match csys with
          | Bot -> (i+1,supp)
          | Csys(sub_csys) -> 
              vect.(i) <- csys;
              if supp = 0 || sub_csys.maximum_support = supp
              then (i+1,sub_csys.maximum_support)
              else Debug.internal_error "[constraint_system.ml >> Row.create] All constraint system in a matrix should have same maximum support"
      ) (0,0) csys_list in
  
    if nb_elt_l = nb_column 
    then { number_column = nb_column; max_support = supp; row = vect }
    else Debug.internal_error "[constraint_system.ml >> Row.create] The number of column indicated does not correspond to the number of elements in the list"

  let get rm k = rm.row.(k-1)
    
  let get_number_column rm = rm.number_column
  
  let get_maximal_support rm = rm.max_support
  
  let iter f rm = Array.iter f rm.row
  
  let fold_right f rm acc = Array.fold_right f rm.row acc
  
  let fold_left f acc rm = Array.fold_left f acc rm.row
  
  let map f rm = 
    let max_s = ref None in
    let vect = Array.make rm.number_column Bot in
    
    for i = 0 to rm.number_column - 1 do
      let new_csys = f rm.row.(i) in
      
      match new_csys,!max_s with
        | Bot, _ -> ()
        | Csys(sub_csys), None -> 
            max_s := Some(sub_csys.maximum_support);
            vect.(i) <- new_csys
        | Csys(sub_csys), Some(s) when s <> sub_csys.maximum_support ->
            Debug.internal_error "[constraint_system.ml >> Row.map] All constraint system produced by [f] should have the same support."
        | _,_ -> vect.(i) <- new_csys
    done;
      
    match !max_s with
      | None -> raise All_bottom
      | Some (s) -> { rm with max_support = s; row = vect }
    
  let map2 f elt_l rm = 
    let max_s = ref None in
    
    let vect = Array.make rm.number_column Bot in
    
    ignore (List.fold_left (fun i elt -> 
      match (f elt rm.row.(i)),!max_s with
        | Bot, _ -> i+1
        | Csys(sub_csys), None -> 
            max_s := Some(sub_csys.maximum_support);
            vect.(i) <- Csys(sub_csys);
            i+1
        | Csys(sub_csys), Some(s) when s <> sub_csys.maximum_support ->
            Debug.internal_error "[constraint_system.ml >> Row.map2] All constraint system produced by [f] should have the same support."
        | csys',_ -> 
            vect.(i) <- csys';
            i+1
    ) 0 elt_l);
     
    match !max_s with
      | None -> raise All_bottom
      | Some s -> { rm with max_support = s; row = vect }
      
  let replace k f rm = 
    let all_bottom = ref true in
    
    let vect = Array.make rm.number_column Bot in
    
    for i = 0 to rm.number_column-1 do
      let new_csys = 
        if i = k-1
        then f rm.row.(i)
        else rm.row.(i)
      in
      
      match new_csys with
        | Bot -> ()
        | Csys(sub_csys) when sub_csys.maximum_support <> rm.max_support -> 
            Debug.internal_error "[constraint_system.ml >> Row.replace] The constraint system replacing should have the same support as the constraint system in the row."
        | _ -> vect.(i) <- new_csys; all_bottom := false      
    done;
    
    if !all_bottom
    then raise All_bottom
    else { rm with row = vect }
    
    
  let check_structure rm = 
    let fst_csys = ref Bot in
  
    for j = 0 to rm.number_column - 1 do
      check_same_structure !fst_csys rm.row.(j);
    
      match !fst_csys, rm.row.(j) with
        | Bot, Csys(_) -> fst_csys := rm.row.(j)
        | _, _ -> ()
    done;
  
    if !fst_csys = Bot
    then Debug.internal_error "[constraint_system.ml >> Row.check_structure_vector] The row matrix is only composed of bottom constraint system."
end

type matrix = row_matrix list
  
module Matrix =
struct

  let empty = []

  let matrix_of_row_matrix row_m = [row_m]
  
  let add_row m row = 
    row::m
  
  (********* Access **********) 
  
  let get_number_column matrix = 
    if matrix = []
    then 0
    else (List.hd matrix).number_column

  let get_number_line = List.length

  let get_maximal_support matrix =
    if matrix = []
    then Debug.internal_error "[Consraint_system.ml >> Matrix.get_maximal_support] The matrix should not be empty."
    else (List.hd matrix).max_support

  (******** Matrix searching *********)      

  let find_in_row_between_col_index i j j' f_test matrix = 
    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if matrix <> [] && (j > j' || j < 0 || j' > (List.hd matrix).number_column)
      then Debug.internal_error (Printf.sprintf "[constraint_system >> find_in_row_between_col_index] The index of the column are not correct : j = %d, j' = %d, column = %d" j j' (List.hd matrix).number_column)
    );
    (***[END DEBUG]***)
  
    let i_line = List.nth matrix (i-1) in
    let not_found = ref true in
    let k = ref (j-1) in
  
    while !not_found && !k < j' do
      if f_test i_line.row.(!k)
      then not_found := false
      else k := !k + 1
    done;
  
    if !not_found 
    then raise Not_found
    else i_line.row.(!k), !k+1
   
  let find_in_row i f_test matrix = 
    if matrix = []
    then raise Not_found
    else find_in_row_between_col_index i 1 (List.hd matrix).number_column f_test matrix
  
  let find_in_col j f_test matrix = 
    let vect = List.find (fun vect -> f_test vect.row.(j-1)) matrix in
    vect.row.(j-1), j
  
  let find_in_col_between_row_index j i i' f_test matrix = 
    let rec search k = function
      | [] -> raise Not_found
      | _::q when k < i -> search (k+1) q
      | vect::q when k >= i && k <= i' -> 
          if f_test vect.row.(j-1)
          then vect.row.(j-1), j
          else search (k+1) q
      | _::_ -> raise Not_found
    in
    search 1 matrix
  
  (******** Iterators *********)  
  
  let iter f_apply matrix = List.iter (Row.iter f_apply) matrix
  
  let iter_row = List.iter
  
  let map f_apply matrix = List.map (Row.map f_apply) matrix
  
  let map_on_column k f_apply matrix = 
    List.fold_right (fun row acc -> 
      try 
        (Row.replace k f_apply row)::acc
      with
        | Row.All_bottom -> acc
    ) matrix []

  let fold_left_on_column j f_apply acc matrix = 
    List.fold_left (fun acc rm -> f_apply acc rm.row.(j-1)) acc matrix
  
  let fold_left_on_row i f_apply acc matrix = 
    let rm = List.nth matrix (i-1) in
    
    Array.fold_left f_apply acc rm.row
  
  let replace_row f matrix = 
    let sup_col = ref None in
  
    List.fold_right (fun rm acc ->           
      List.fold_right (fun rm' acc' ->
        match !sup_col with
          | None -> 
              sup_col := Some(rm'.max_support, rm'.number_column);
              rm'::acc'
          | Some(s,nb) when rm'.max_support <> s || rm'.number_column <> nb -> 
              Debug.internal_error "[constraint_system >> Matrix.replace_row] The function did not produce row matrices with same number of columns or same maximal support."
          | _ -> rm'::acc'
      ) (f rm) acc
    ) matrix []
    
  let fold_right_row = List.fold_right 
  
  let fold_left_row = List.fold_left
    
  (******** Matrix scanning *********)

  let is_empty matrix = matrix = []

  let exists_in_row_between_col_index i j j' f_test matrix = 
    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if matrix <> [] && (j > j' || j < 0 || j' > (List.hd matrix).number_column)
      then Debug.internal_error "[constraint_system >> Matrix.exists_in_row_between_col_index] The index of the column are not correct"
    );
    (***[END DEBUG]***)
  
    let i_line = List.nth matrix (i-1) in
    let not_found = ref true in
    let k = ref (j-1) in
  
    while !not_found && !k < j' do
      if f_test i_line.row.(!k)
      then not_found := false
      else k := !k + 1
    done;
  
    not !not_found
  
  let exists_in_row i f_test matrix = 
    if matrix = [] 
    then false
    else exists_in_row_between_col_index i 1 (List.hd matrix).number_column f_test matrix
  
  let exists_in_col j f_test matrix = 
    List.exists (fun rm -> f_test rm.row.(j-1)) matrix
  
  let exists_in_col_between_row_index j i i' f_test matrix = 
    let rec search k = function
      | [] -> false
      | _::q when k < i -> search (k+1) q
      | rm::q when k >= i && k <= i' -> 
          if f_test rm.row.(j-1)
          then  true
          else search (k+1) q
      | _::_ -> false
    in
    search 1 matrix
    
  let check_structure matrix =
    if matrix = []
    then ()
    else
      begin
        List.iter Row.check_structure matrix;
  
        let fst_csys = ref Bot in
  
        iter (fun csys ->
          check_same_shape csys !fst_csys;
    
          match !fst_csys, csys with
            | Bot, Csys(_) -> fst_csys := csys
            | _, _ -> ()
        ) matrix;
        
        let s, nb_col = (List.hd matrix).max_support, (List.hd matrix).number_column in
        
        List.iter (fun rm ->
          if rm.number_column <> nb_col || rm.max_support <> s
          then Debug.internal_error "[constraint_system >> Matrix.check_structure] The rows of the matrix do not have the same number of column or same maximal support"
        ) (List.tl matrix)
      end
  
  let exists_bot_line matrix = 
    List.exists (fun rm ->
      let all_bot = ref true in
      
      for j = 0 to rm.number_column - 1 do
        if rm.row.(j) <> Bot
        then all_bot := false
      done;
      
      !all_bot
    ) matrix
    
  let normalise m =
    List.fold_right (fun row acc -> 
      try
        (Row.map (fun csys -> 
          if is_unsatisfiable csys then Bot else csys) row)::acc
      with
        Row.All_bottom -> acc
    ) m []
    
  let display m = 
    if m = []
    then Printf.sprintf "Matrix of constraint system = Empty"
    else
      begin
        let result = ref (Printf.sprintf "Matrix of constraint system (nb_column = %d) = \n" (List.hd m).number_column) in
        let i = ref 1 in
    
        List.iter (fun rm ->
          for j = 1 to rm.number_column do
            result := Printf.sprintf "%sConstraint_system(%d,%d) = %s\n\n" !result !i j (display rm.row.(j-1))
          done;
          i := !i+1
        ) m;
        !result
      end
      
  
end
  
(***************************************
***          Rule applications       ***
****************************************) 

exception Not_applicable

let apply_rule_on_vector search_and_apply_rule apply_rule vect nb_col =
  let left_vec = Array.make nb_col Bot
  and right_vec = Array.make nb_col Bot in
  
  let j = ref 0
  and left_full_bot = ref true
  and right_full_bot = ref true
  and stay_loop = ref true in
  
  try 
    while !stay_loop && !j < nb_col do
      match vect.(!j) with
        | Bot -> j := !j + 1
        | Csys(_) -> stay_loop := false          
    done;
  
    if !j = nb_col
    then None,None
    else 
      begin
        let position, left_csys, right_csys = search_and_apply_rule vect.(!j) in
          
        left_vec.(!j) <- left_csys;
        right_vec.(!j) <- right_csys;
        
        let max_supp = get_maximal_support vect.(!j) in
        
        (***[BEGIN DEBUG]***)
        Debug.low_debugging (fun () ->
          if not (is_bottom left_csys) && (get_maximal_support left_csys) <> max_supp
          then Debug.internal_error "[constraint_system >> apply_rule_on_vector] The maximal support of the left constraint produced is not the same as the maximal support of the vector  (1)";
          
          if not (is_bottom right_csys) && (get_maximal_support right_csys) <> max_supp
          then Debug.internal_error "[constraint_system >> apply_rule_on_row_matrix] The maximal support of the right constraint produced is not the same as the maximal support of the vector (1)"
        );
        (***[END DEBUG]***)
          
        if left_csys <> Bot
        then left_full_bot := false;
          
        if right_csys <> Bot
        then right_full_bot := false;
          
        j := !j + 1;
    
        while !j < nb_col do
          match vect.(!j) with
            | Bot -> j := !j + 1
            | Csys(_) -> 
      
                let left_csys, right_csys = apply_rule position vect.(!j) in
          
                left_vec.(!j) <- left_csys;
                right_vec.(!j) <- right_csys;
                
                (***[BEGIN DEBUG]***)
                Debug.low_debugging (fun () ->
                  if not (is_bottom left_csys) && (get_maximal_support left_csys) <> max_supp
                  then Debug.internal_error "[constraint_system >> apply_rule_on_row_matrix] The maximal support of the left constraint produced is not the same as the maximal support of the row matrix (2)";
          
                  if not (is_bottom right_csys) && (get_maximal_support right_csys) <> max_supp
                  then Debug.internal_error "[constraint_system >> apply_rule_on_row_matrix] The maximal support of the right constraint produced is not the same as the maximal support of the row matrix (2)"
                );
                (***[END DEBUG]***)
          
                if left_csys <> Bot
                then left_full_bot := false;
          
                if right_csys <> Bot
                then right_full_bot := false;
          
                j := !j + 1
        done;
  
        (if !left_full_bot then None else Some(left_vec)),(if !right_full_bot then None else Some(right_vec))
      end
  with
    Not_applicable -> None, Some(vect)
    
let apply_rule_on_row_matrix search_and_apply_rule apply_rule row =
  let (vect_l, vect_r) = apply_rule_on_vector search_and_apply_rule apply_rule row.row row.number_column in
  
  let vect_l' = match vect_l with
    | Some(v) -> Some({row with row = v})
    | None -> None
   
  and vect_r' = match vect_r with
    | Some(v) -> Some({row with row = v})
    | None -> None
  in
  
  vect_l', vect_r'
 
let apply_internal_rule search_and_apply_rule apply_rule i_line matrix =
  
  let rec search_line k line_list = match line_list with
    | [] -> Debug.internal_error "[constraint_system >> apply_internal_rule] The index is not a line index of the matrix."
    | line::q when i_line = k -> 
        begin match apply_rule_on_row_matrix search_and_apply_rule apply_rule line with
          | None,None -> q
          | Some(l_vec), None -> l_vec::q
          | None,Some(r_vec) -> r_vec::q
          | Some(l_vec),Some(r_vec) -> l_vec::r_vec::q
        end
    | line::q -> line::(search_line (k+1) q)
  in
  
  search_line 1 matrix
    
let apply_internal_rule_full_column search_and_apply_rule apply_rule matrix = 
  
  let rec search_line line_list = match line_list with
    | [] -> []
    | line::q -> 
        begin match apply_rule_on_row_matrix search_and_apply_rule apply_rule line with
          | None,None -> (search_line q)
          | Some(l_vec), None -> l_vec::(search_line q)
          | None,Some(r_vec) -> r_vec::(search_line q)
          | Some(l_vec),Some(r_vec) -> l_vec::r_vec::(search_line q)
        end
  in
  
  search_line matrix
  
  
let apply_external_rule search_and_apply_rule apply_rule matrix = 

  let rec go_through line_list = match line_list with
    | [] -> [],[]
    | line::q ->
        let (left_list, right_list) = go_through q in
        
        let (left_vec,right_vec) = apply_rule_on_row_matrix search_and_apply_rule apply_rule line in
        
        let left_list' = match left_vec with
          | None -> left_list
          | Some(vec) -> vec::left_list
          
        and right_list' = match right_vec with
          | None -> right_list
          | Some(vec) -> vec::right_list in
          
        (left_list',right_list')
  in
  
  let left_matrix, right_matrix = go_through matrix in
  
  Debug.high_debugging (fun () ->
    if Matrix.exists_bot_line left_matrix
    then Debug.internal_error "[constraint_system.ml >> apply_external_rule] There exists a line with only Bot in the left matrix";
    
    Matrix.check_structure left_matrix
  );
  
  Debug.high_debugging (fun () ->
    if Matrix.exists_bot_line right_matrix
    then Debug.internal_error "[constraint_system.ml >> apply_external_rule] There exists a line with only Bot in the right matrix";
    
    Matrix.check_structure right_matrix
  );
  
  left_matrix,right_matrix
