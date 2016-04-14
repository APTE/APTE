open Standard_library

(***********************************
***           Rule Cons          ***
************************************)

(* Application function *)

let apply_function_cons is_phase_1 support recipe_var symbol recipe_var_sons =
  let unify_and_apply_message_equations, apply_recipe_substitution, deducibility_search_and_replace2, deducibility_replace2 =
    if is_phase_1
    then
      Constraint_system.Phase_1.unify_and_apply_message_equations,
      Constraint_system.Phase_1.apply_recipe_substitution,
      Constraint_system.Phase_1.deducibility_search_and_replace2,
      Constraint_system.Phase_1.deducibility_replace2
    else
      Constraint_system.Phase_2.unify_and_apply_message_equations,
      Constraint_system.Phase_2.apply_recipe_substitution,
      Constraint_system.Phase_2.deducibility_search_and_replace2,
      Constraint_system.Phase_2.deducibility_replace2
  in

  (* Get the informations *)
  let arity = Term.get_arity symbol in

  (* Create the fresh first order variables x1,...xn *)
  let fresh_term_vars = Term.fresh_variable_list2 Term.Existential arity in

  (* Create the list of constraints with the second order variable rvar_sons *)
  let f_apply cons =
    let left_set = List.map2 (fun r m -> Constraint.Deducibility.create r (Recipe.get_support r) m) recipe_var_sons fresh_term_vars in
    let right_set = [Constraint.Deducibility.add_noCons cons symbol] in

    left_set,right_set
  in

  let f_test cons = Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable cons) recipe_var in

  let f_after cons left_csys right_csys =
    (* Create the term f(x1,...,xn) and recipe f(X_1,...,X_n)*)
    let message_eq = Term.apply_function symbol fresh_term_vars
    and recipe_eq = Recipe.apply_function symbol (List.map Recipe.recipe_of_variable recipe_var_sons) in

    (* The left constraint system *)

    let left_csys' =
      try
        let left_csys_1 = unify_and_apply_message_equations left_csys [Constraint.Deducibility.get_message cons,message_eq] in
        let left_csys_2 = apply_recipe_substitution left_csys_1 (Recipe.create_substitution recipe_var recipe_eq) in
        if Constraint_system.is_unsatisfiable left_csys_2
        then Constraint_system.bottom
        else left_csys_2
      with
        Term.Not_unifiable -> Constraint_system.bottom
    in

    let right_csys' =
      let cons' = Constraint.Deducibility.add_noCons cons symbol in
      (* Check if the constraint system is satisfiable *)
      if Constraint.Deducibility.is_unsatisfiable (Constraint_system.get_frame right_csys) cons'
      then Constraint_system.bottom
      else right_csys
    in

    left_csys',right_csys'
  in

  let search_and_apply csys =
    let cons, position, left_csys, right_csys = deducibility_search_and_replace2 csys (Constraint.SUnique support) f_test f_apply  in
    let left_csys',right_csys' = f_after cons left_csys right_csys in
    position,left_csys',right_csys'

  and apply position csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom,Constraint_system.bottom
    else
      let cons, left_csys, right_csys = deducibility_replace2 csys position f_apply in
      f_after cons left_csys right_csys
  in

  search_and_apply, apply

(* Internal application of the rule cons *)

let apply_cons_row_matrix var symbol row =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Row.check_structure row
  );
  (***[END DEBUG]***)

  let support = Recipe.get_support var in
  let arity = Term.get_arity symbol in

  let recipe_var_sons = Recipe.fresh_variable_list arity support in

  let search_and_apply, apply = apply_function_cons true support var symbol recipe_var_sons in

  Constraint_system.apply_rule_on_row_matrix search_and_apply apply row

(* External application of the rule cons in phase 1 *)

let apply_external_cons_phase_1 var symbol matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let support = Recipe.get_support var in
  let arity = Term.get_arity symbol in

  let recipe_var_sons = Recipe.fresh_free_variable_list arity support in

  let search_and_apply, apply = apply_function_cons true support var symbol recipe_var_sons in

  Constraint_system.apply_external_rule search_and_apply apply matrix

(* External application of the rule cons in phase 2 *)

let apply_external_cons_phase_2 var symbol matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let support = Recipe.get_support var in
  let arity = Term.get_arity symbol in

  let recipe_var_sons = Recipe.fresh_free_variable_list arity support in

  let search_and_apply, apply = apply_function_cons false support var symbol recipe_var_sons in

  Constraint_system.apply_external_rule search_and_apply apply matrix

(**********************************
***           Rule axiom        ***
***********************************)

(* Application function *)

let apply_function_axiom is_phase_1 support_frame_elt path recipe_var =
  let unify_and_apply_message_equations, apply_recipe_substitution, deducibility_search_and_replace2, deducibility_replace2 =
    if is_phase_1
    then
      Constraint_system.Phase_1.unify_and_apply_message_equations,
      Constraint_system.Phase_1.apply_recipe_substitution,
      Constraint_system.Phase_1.deducibility_search_and_replace2,
      Constraint_system.Phase_1.deducibility_replace2
    else
      Constraint_system.Phase_2.unify_and_apply_message_equations,
      Constraint_system.Phase_2.apply_recipe_substitution,
      Constraint_system.Phase_2.deducibility_search_and_replace2,
      Constraint_system.Phase_2.deducibility_replace2
  in

  (* Create the list of constraints *)
  let f_apply frame_pos cons = [], [Constraint.Deducibility.add_noAxiom cons frame_pos] in

  let f_test cons = Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable cons) recipe_var in

  let f_after cons frame_pos frame_elt left_csys right_csys =
    (* The left constraint system *)
    let left_csys' =
      try
        let left_csys_1 = unify_and_apply_message_equations left_csys [Constraint.Deducibility.get_message cons,Constraint.Frame.get_message frame_elt] in
        let left_csys_2 = apply_recipe_substitution left_csys_1 (Recipe.create_substitution recipe_var (Constraint.Frame.get_recipe frame_elt)) in
        if Constraint_system.is_unsatisfiable left_csys_2
        then Constraint_system.bottom
        else left_csys_2
      with
        Term.Not_unifiable -> Constraint_system.bottom
    in

    let right_csys' =
      let cons' = Constraint.Deducibility.add_noAxiom cons frame_pos in
      (* Check if the constraint system is satisfiable *)
      if Constraint.Deducibility.is_unsatisfiable (Constraint_system.get_frame right_csys) cons'
      then Constraint_system.bottom
      else right_csys
    in

    left_csys',right_csys'
  in

  let search_and_apply csys =
    let frame_elt, pos_frame =
      try
        Constraint.search (Constraint.SUnique support_frame_elt) (fun fc ->
          Recipe.is_path_of_recipe (Constraint.Frame.get_recipe fc) path
        ) (Constraint_system.get_frame csys)
      with
        Not_found -> raise Constraint_system.Not_applicable
    in

    if Constraint.Frame.is_noUse frame_elt
    then raise Constraint_system.Not_applicable;

    if Recipe.occurs recipe_var (Constraint.Frame.get_recipe frame_elt)
    then raise Constraint_system.Not_applicable;

    let cons, pos_cons, left_csys,right_csys = deducibility_search_and_replace2 csys (Constraint.SUnique (Recipe.get_support recipe_var)) f_test (f_apply pos_frame) in
    let left_csys',right_csys'  = f_after cons pos_frame frame_elt left_csys right_csys in
    (pos_frame, pos_cons), left_csys', right_csys'

  and apply (pos_frame,pos_cons) csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom,Constraint_system.bottom
    else
        let frame_elt = Constraint.get pos_frame (Constraint_system.get_frame csys) in
        let cons, left_csys,right_csys = deducibility_replace2 csys pos_cons (f_apply pos_frame)  in
        f_after cons pos_frame frame_elt left_csys right_csys
  in

  search_and_apply, apply

(* Internal application *)

let apply_axiom_row_matrix support var path row =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Row.check_structure row
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_axiom true support path var in
  Constraint_system.apply_rule_on_row_matrix search_and_apply apply row

let apply_external_axiom_phase_1 support var path matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_axiom true support path var in
  Constraint_system.apply_external_rule search_and_apply apply matrix

let apply_external_axiom_phase_2 support var path matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_axiom false support path var in
  Constraint_system.apply_external_rule search_and_apply apply matrix


(*******************************************
***                Rule dest             ***
********************************************)

(* Application function *)
let apply_function_dest support_frame_elt path support_rule dest_symbol recipe_sons =

  (* Create the frame *)
  let f_apply frame_elt = [Constraint.Frame.add_yesDest frame_elt],[Constraint.Frame.add_noDest frame_elt dest_symbol support_rule] in

  (* Test function *)
  let f_test frame_elt =
    if Recipe.is_path_of_recipe (Constraint.Frame.get_recipe frame_elt) path
    then
      if Constraint.Frame.is_noUse frame_elt || Constraint.Frame.is_yesDest frame_elt
      then raise Constraint_system.Not_applicable
      else true
     else false in

  let f_after frame_elt left_csys right_csys =
    (* The left constraint system *)

    let left_csys,list_new_fc_with_var =
      let lhs,rhs = Term.fresh_rewrite_rule dest_symbol in

      (* Create the new constraints *)
      let list_cons = List.map2 (fun r t -> Constraint.Deducibility.create r support_rule t) recipe_sons  (List.tl lhs) in

      (* Crete the new frame elt *)
      let recipe = Recipe.apply_function dest_symbol ((Constraint.Frame.get_recipe frame_elt)::(List.map Recipe.recipe_of_variable recipe_sons)) in
      let new_frame_elt = Constraint.Frame.create recipe support_rule rhs in

      (* Detection of Eqlr application *)

      let frame_elt_message = Constraint.Frame.get_message frame_elt in

      let list_new_fc_with_var =
        if Term.is_function frame_elt_message
        then
          begin
            let arg = Term.nth_args frame_elt_message 1 in
            if Term.is_variable arg
            then
              let (cons,_) = Constraint.search (Constraint.SUntil (support_frame_elt - 1))
                (fun cons ->
                  Recipe.is_free_variable (Constraint.Deducibility.get_recipe_variable cons)
                  && Term.is_equal_term arg (Constraint.Deducibility.get_message cons)
                ) (Constraint_system.get_deducibility_constraint_set left_csys) in
              [(Recipe.apply_function_to_path dest_symbol path,(Constraint.Deducibility.get_recipe_variable cons))]
            else []
          end
        else []
      in

      (* Creation of the constraint systems *)

      let left_csys_1 =
        Constraint_system.add_deducibility_constraint (
          Constraint_system.add_frame_constraint left_csys  [new_frame_elt]
          ) list_cons
      in

      try
        Constraint_system.Phase_1.unify_and_apply_message_equations left_csys_1 [frame_elt_message,List.hd lhs], list_new_fc_with_var
      with
        Term.Not_unifiable -> Constraint_system.bottom, []
    in

    (* The right constraint system: Stay unchanged *)

    left_csys, right_csys, list_new_fc_with_var
  in

  let search_and_apply ref_path_var csys =
    try
      let frame_elt, pos_frame, left_csys, right_csys = Constraint_system.frame_search_and_replace2 csys (Constraint.SUnique support_frame_elt) f_test f_apply in

      let left_csys', right_csys', list_path_var = f_after frame_elt left_csys right_csys in

      (* Incorporate the new frame elements with variable as messages *)

      List.iter (fun (path,var) ->
        if List.exists (fun (p,v) -> Recipe.is_equal_path p path && Recipe.is_equal_variable var v) !ref_path_var
        then ()
        else ref_path_var := (path,var)::!ref_path_var
      ) list_path_var;

      pos_frame, left_csys', right_csys'
    with
      Not_found -> raise Constraint_system.Not_applicable

  and apply ref_path_var pos_frame csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom,Constraint_system.bottom
    else
      let frame_elt, left_csys, right_csys = Constraint_system.frame_replace2 csys pos_frame f_apply in

      let left_csys',right_csys', list_path_var = f_after frame_elt left_csys right_csys in

      (* Incorporate the new frame elements with variable as messages *)

      List.iter (fun (path,var) ->
        if List.exists (fun (p,v) -> Recipe.is_equal_path p path && Recipe.is_equal_variable var v) !ref_path_var
        then ()
        else ref_path_var := (path,var)::!ref_path_var
      ) list_path_var;

      left_csys',right_csys'
  in

  search_and_apply, apply


let apply_full_column_dest support_frame path support_rule dest_symbol matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let arity = Term.get_arity dest_symbol in
  let recipe_var_sons = Recipe.fresh_variable_list (arity-1) support_rule in

  let ref_path_var = ref [] in

  let search_and_apply, apply = apply_function_dest support_frame path support_rule dest_symbol recipe_var_sons in

  let matrix' = Constraint_system.apply_internal_rule_full_column (search_and_apply ref_path_var) (apply ref_path_var) matrix in

  matrix', !ref_path_var

(***********************************************
***      Rule dest : Specific for Tuple      ***
************************************************)

let apply_function_proj support_frame_elt path tuple_symbol=
  (* Test function *)
  let f_test frame_elt =
    if Recipe.is_path_of_recipe (Constraint.Frame.get_recipe frame_elt) path
    then
      if Constraint.Frame.is_noUse frame_elt || Constraint.Frame.is_yesDest frame_elt
      then raise Constraint_system.Not_applicable
      else true
     else false in

  (* Create the frame *)
  let f_apply fc = [Constraint.Frame.add_yesDest fc] in

  let proj_symb_list = Term.get_projections tuple_symbol in

  let f_after original_csys frame_elt left_csys =
    (* The left constraint system *)

    let frame_elt_message = Constraint.Frame.get_message frame_elt in

    if Term.is_function frame_elt_message && Term.is_equal_symbol tuple_symbol (Term.top frame_elt_message)
    then
      begin
        (* Detection of Eqlr application *)

        let arg_list = Term.get_args frame_elt_message in

        let list_new_fc_with_var =
          List.fold_left2 (fun acc proj_symb arg ->
            if Term.is_variable arg
            then
              let (cons,_) = Constraint.search (Constraint.SUntil (support_frame_elt - 1))
                (fun cons ->
                  Recipe.is_free_variable (Constraint.Deducibility.get_recipe_variable cons)
                  && Term.is_equal_term arg (Constraint.Deducibility.get_message cons)
                ) (Constraint_system.get_deducibility_constraint_set left_csys) in
              (Recipe.apply_function_to_path proj_symb path,(Constraint.Deducibility.get_recipe_variable cons))::acc
            else acc
          ) [] proj_symb_list arg_list
        in

        (* Build of the left constraint system *)

        let recipe_list = List.map (fun proj_symb -> Recipe.apply_function proj_symb [Constraint.Frame.get_recipe frame_elt]) proj_symb_list in
        let frame_elt_list = List.map2 (fun r sub_t -> Constraint.Frame.create r support_frame_elt sub_t) recipe_list arg_list in

        let left_csys_1 = Constraint_system.add_frame_constraint left_csys frame_elt_list in

        left_csys_1,Constraint_system.bottom, list_new_fc_with_var
      end
    else
      Constraint_system.bottom, original_csys, []
  in

  let search_and_apply ref_path_var csys =
    try
      let frame_elt, pos_frame, left_csys = Constraint_system.frame_search_and_replace csys (Constraint.SUnique support_frame_elt) f_test f_apply in

      let left_csys',right_csys', list_path_var = f_after csys frame_elt left_csys in

      (* Incorporate the new frame elements with variable as messages *)

      List.iter (fun (path,var) ->
        if List.exists (fun (p,v) -> Recipe.is_equal_path p path  && Recipe.is_equal_variable var v) !ref_path_var
        then ()
        else ref_path_var := (path,var)::!ref_path_var
      ) list_path_var;

      pos_frame, left_csys',right_csys'
    with
      Not_found -> raise Constraint_system.Not_applicable

  and apply ref_path_var pos_frame  csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom,Constraint_system.bottom
    else
      let frame_elt, left_csys = Constraint_system.frame_replace csys pos_frame f_apply in

      let left_csys',right_csys', list_path_var = f_after csys frame_elt left_csys in

      (* Incorporate the new frame elements with variable as messages *)

      List.iter (fun (path,var) ->
        if List.exists (fun (p,v) -> Recipe.is_equal_path p path  && Recipe.is_equal_variable var v) !ref_path_var
        then ()
        else ref_path_var := (path,var)::!ref_path_var
      ) list_path_var;

      left_csys',right_csys'
  in

  search_and_apply, apply

let apply_full_column_dest_tuple support_frame path tuple_symbol matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_proj support_frame path tuple_symbol in

  let ref_path_var = ref [] in

  let matrix' = Constraint_system.apply_internal_rule_full_column (search_and_apply ref_path_var) (apply ref_path_var) matrix in

  matrix', !ref_path_var

(***********************************************
***                  Rule eqll               ***
************************************************)

(* For this rule, we will apply all the possible test at the same time *)

let generate_equation_list support csys =

  let frame = Constraint_system.get_frame csys in

  let fc_list_inf_supp =
    Constraint.fold_left (Constraint.SUntil (support-1)) (fun acc fc ->
      if Constraint.Frame.is_noUse fc
      then acc
      else (Constraint.Frame.get_message fc)::acc
    ) [] frame
  in

  let _, equations_list =
    Constraint.fold_left (Constraint.SUnique support) (fun (prev_terms,equations) fc ->
      if Constraint.Frame.is_noUse fc
      then (prev_terms,equations)
      else
        let t = Constraint.Frame.get_message fc in
        (t::prev_terms, (List.fold_right (fun pt eq -> (pt,t)::eq) prev_terms equations))
    ) (fc_list_inf_supp,[]) frame
  in

  equations_list


type input_eqll =
  {
    current_eq : Term.substitution;
    current_neq : Term.formula list;
    equation_to_solve : (Term.term * Term.term) list
  }

(** [generate_input s r_m] generates the [input_eqll] of the row matrix [r_m].
    Typically, [r_m] is the initial row matrix on which we apply the rule {% \Eqll. %} *)
let generate_input support row_matrix =
  Constraint_system.Row.fold_right (fun csys acc ->
    if Constraint_system.is_bottom csys
    then None:: acc
    else
      let input =
        {
          current_eq = Term.identity;
          current_neq = [];
          equation_to_solve = generate_equation_list support csys
        } in

      Some(input):: acc
  ) row_matrix []

exception No_more

let generate_row_matrices support row_matrix =
  let list_input = generate_input support row_matrix in

  (* [generate_eq_neq] generates the new lists of input corresponding to one application
      of the rule {% \Eqll %} on a row matrix. The state of the row matrix is given by the
      list of inputs given as argument. *)
  let rec generate_eq_neq = function
    | [] -> true,true, [],[]
    | None::q ->
        let (all_none_eq,all_none_neq,l_eq,l_neq) = generate_eq_neq q in
        (all_none_eq,all_none_neq,None::l_eq,None::l_neq)
    | Some(input)::q ->
        let (all_none_eq,all_none_neq,l_eq,l_neq) = generate_eq_neq q in

        if input.equation_to_solve = []
        then raise No_more
        else
          try
            let subst = Term.unify [List.hd input.equation_to_solve] in
            if Term.is_identity subst
            then
              let eq = Some({ input with equation_to_solve = List.tl input.equation_to_solve })
              and neq = None in
              (false,all_none_neq,eq::l_eq,neq::l_neq)
            else
              let eq = Some(
                { input with
                  current_eq = Term.compose input.current_eq subst;
                  equation_to_solve = Term.apply_substitution subst (List.tl input.equation_to_solve) (fun l f_apply ->
                    List.map (fun (t1,t2) -> f_apply t1, f_apply t2) l)
                })
              and neq = Some(
                { input with
                  current_neq = (Term.create_disjunction_inequation (Term.equations_of_substitution subst))::input.current_neq;
                  equation_to_solve = List.tl input.equation_to_solve
                })
              in
              (false,false,eq::l_eq,neq::l_neq)
          with
            Term.Not_unifiable ->
              let neq = Some(
                { input with
                  equation_to_solve = List.tl input.equation_to_solve
                }) in
              (all_none_eq, false, None::l_eq,neq::l_neq)
  in

  let apply_input input csys =
    let csys' = List.fold_left Constraint_system.add_message_formula csys input.current_neq in

    Constraint_system.Phase_1.apply_message_substitution csys' input.current_eq
  in

  let ref_list_row_matrix = ref [] in

  let rec generate_csys list_input =
    try
      let (all_none_eq,all_none_neq, eq_l,neq_l) = generate_eq_neq list_input in
      begin match all_none_eq, all_none_neq with
        |true,true -> ()
        |true,false -> generate_csys neq_l
        |false,true -> generate_csys eq_l
        |_,_ ->
          (generate_csys eq_l);
          (generate_csys neq_l)
      end
    with No_more ->
      begin
        try
          let row_matrix' =
          Constraint_system.Row.map2 (fun input_o csys -> match input_o with
            | None -> Constraint_system.bottom
            | Some(input) -> apply_input input csys
          ) list_input row_matrix in

          ref_list_row_matrix := row_matrix' :: !ref_list_row_matrix
        with Constraint_system.Row.All_bottom -> ()
      end
  in

  generate_csys list_input;

  !ref_list_row_matrix

let apply_eqll support matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  Constraint_system.Matrix.replace_row (generate_row_matrices support) matrix

(**********************************
***           Rule eqlr         ***
***********************************)

(* Application of the rule *)

let apply_function_eqlr support_frame_elt path var =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if support_frame_elt <= Recipe.get_support var
    then Debug.internal_error "[rule.ml >> apply_function_eqlr] The support of the recipe variable should be strictly inferior to the support of the frame constraint"
  );
  (***[END DEBUG]***)

  let f_apply frame_elt = [Constraint.Frame.add_noUse frame_elt] in

  let f_test frame_elt =
    if Recipe.is_path_of_recipe (Constraint.Frame.get_recipe frame_elt) path
    then
      if Constraint.Frame.is_noUse frame_elt
      then raise Constraint_system.Not_applicable
      else true
    else false in

  let f_after original_csys cons frame_elt left_csys =
    try
      let subst = Term.unify [Constraint.Frame.get_message frame_elt,Constraint.Deducibility.get_message cons] in

      if Term.is_identity subst
      then left_csys, Constraint_system.bottom
      else
        let left_csys_1 = Constraint_system.Phase_1.apply_message_substitution left_csys subst in
        let formula = Term.create_disjunction_inequation (Term.equations_of_substitution subst) in
        let right_csys = Constraint_system.add_message_formula original_csys formula in
        left_csys_1,right_csys

    with Term.Not_unifiable ->
      Constraint_system.bottom,original_csys
  in

  let search_and_apply csys =
    let cons,pos_cons =
      Constraint.search
        (Constraint.SUnique (Recipe.get_support var))
        (fun cons -> Recipe.is_equal_variable var (Constraint.Deducibility.get_recipe_variable cons))
        (Constraint_system.get_deducibility_constraint_set csys) in

    try
      let frame_elt, pos_frame, left_csys = Constraint_system.frame_search_and_replace csys (Constraint.SUnique support_frame_elt) f_test f_apply in

      let left_csys', right_csys' = f_after csys cons frame_elt left_csys in
      (pos_cons,pos_frame), left_csys', right_csys'
    with
      Not_found -> raise Constraint_system.Not_applicable

  and apply (pos_cons,pos_frame) csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom, Constraint_system.bottom
    else
      let cons = Constraint.get pos_cons (Constraint_system.get_deducibility_constraint_set csys) in
      let frame_elt, left_csys = Constraint_system.frame_replace csys pos_frame f_apply in
      f_after csys cons frame_elt left_csys
  in

  search_and_apply, apply

let apply_full_column_eqlr support_frame path variable matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_eqlr support_frame path variable in
  Constraint_system.apply_internal_rule_full_column search_and_apply apply matrix

(**********************************
***        Rule eqlr frame      ***
***********************************)

let apply_function_eqlr_frame support_1 path_1 support_2 path_2 =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if support_1 <= support_2
    then Debug.internal_error "[rule.ml >> apply_function_eqlr_frame] The second support should be strictly inferior to the first one"
  );
  (***[END DEBUG]***)

  let f_apply frame_elt = [Constraint.Frame.add_noUse frame_elt] in

  let f_test frame_elt =
    if Recipe.is_path_of_recipe (Constraint.Frame.get_recipe frame_elt) path_1
    then
      if Constraint.Frame.is_noUse frame_elt
      then raise Constraint_system.Not_applicable
      else true
    else false in

  let f_after original_csys frame_elt_1 frame_elt_2 left_csys =
    try
      let subst = Term.unify [Constraint.Frame.get_message frame_elt_1,Constraint.Frame.get_message frame_elt_2] in

      if Term.is_identity subst
      then left_csys, Constraint_system.bottom
      else
        let left_csys_1 = Constraint_system.Phase_1.apply_message_substitution left_csys subst in
        let formula = Term.create_disjunction_inequation (Term.equations_of_substitution subst) in
        let right_csys = Constraint_system.add_message_formula original_csys formula in
        left_csys_1,right_csys

    with Term.Not_unifiable ->
      Constraint_system.bottom,original_csys
  in

  let search_and_apply csys =
    let frame_elt_2,position_2 =
      Constraint.search
        (Constraint.SUnique support_2)
        (fun fc -> Recipe.is_path_of_recipe (Constraint.Frame.get_recipe fc) path_2)
        (Constraint_system.get_frame csys) in

    try
      let frame_elt_1, position_1, left_csys = Constraint_system.frame_search_and_replace csys (Constraint.SUnique support_1) f_test f_apply in

      let left_csys', right_csys' = f_after csys frame_elt_1 frame_elt_2 left_csys in
      (position_1,position_2), left_csys', right_csys'
    with
      Not_found -> raise Constraint_system.Not_applicable

  and apply (position_1,position_2) csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom, Constraint_system.bottom
    else
      let frame_elt_2 = Constraint.get position_2 (Constraint_system.get_frame csys) in
      let frame_elt_1, left_csys = Constraint_system.frame_replace csys position_1 f_apply in
      f_after csys frame_elt_1 frame_elt_2 left_csys
  in

  search_and_apply, apply

let apply_full_column_eqlr_frame support_1 path_1 support_2 path_2 matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_eqlr_frame support_1 path_1 support_2 path_2 in
  Constraint_system.apply_internal_rule_full_column search_and_apply apply matrix

(**********************************
***       Rule eqrr Phase 1     ***
***********************************)

let apply_function_eqrr_phase_1 var1 var2 =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if Recipe.get_support var1 < Recipe.get_support var2
    then Debug.internal_error "[rule.ml >> apply_function_eqrr_phase_1] The second support should be inferior to the first one";

    if not (Recipe.is_free_variable var2)
    then Debug.internal_error "[rule.ml >> apply_function_eqrr_phase_1] The second variable should be free"
  );
  (***[END DEBUG]***)

  let f_test var cons = Recipe.is_equal_variable var (Constraint.Deducibility.get_recipe_variable cons) in

  let subst_r = Recipe.create_substitution var1 (Recipe.recipe_of_variable var2) in

  (* cons1 is the constraint with the biggest support *)
  let f_after original_csys cons1 cons2 left_csys =
    try
      let subst = Term.unify [Constraint.Deducibility.get_message cons1,Constraint.Deducibility.get_message cons2] in

      if Term.is_identity subst
      then
        let left_csys' = Constraint_system.Phase_1.apply_recipe_substitution left_csys subst_r in
        if Constraint_system.is_unsatisfiable left_csys'
        then Constraint_system.bottom, Constraint_system.bottom
        else left_csys', Constraint_system.bottom
      else
        let left_csys' = Constraint_system.Phase_1.apply_message_substitution left_csys subst in
        let left_csys'' = Constraint_system.Phase_1.apply_recipe_substitution left_csys' subst_r in

        let formula = Term.create_disjunction_inequation (Term.equations_of_substitution subst) in
        let right_csys = Constraint_system.add_message_formula original_csys formula in
        if Constraint_system.is_unsatisfiable left_csys''
        then Constraint_system.bottom, right_csys
        else left_csys'', right_csys

    with Term.Not_unifiable ->
      Constraint_system.bottom,original_csys
  in

  let search_and_apply csys =
    let cons1,pos_cons1,left_csys = Constraint_system.Phase_1.deducibility_search_and_replace csys (Constraint.SUnique (Recipe.get_support var1)) (f_test var1) (fun _ -> []) in
    let cons2,pos_cons2 = Constraint.search (Constraint.SUnique (Recipe.get_support var2)) (f_test var2) (Constraint_system.get_deducibility_constraint_set csys) in
    let left_csys', right_csys' = f_after csys cons1 cons2 left_csys in
    (pos_cons1,pos_cons2), left_csys', right_csys'

  and apply (pos_cons1,pos_cons2) csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom, Constraint_system.bottom
    else
      let cons1, left_csys = Constraint_system.Phase_1.deducibility_replace csys pos_cons1 (fun _ -> []) in
      let cons2 = Constraint.get pos_cons2 (Constraint_system.get_deducibility_constraint_set csys) in
      f_after csys cons1 cons2 left_csys
  in

  search_and_apply, apply

let apply_eqrr_row_matrix var1 var2 row =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Row.check_structure row
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_eqrr_phase_1 var1 var2 in
  Constraint_system.apply_rule_on_row_matrix search_and_apply apply row

let apply_external_eqrr_phase_1 var1 var2 matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_eqrr_phase_1 var1 var2 in
  Constraint_system.apply_external_rule search_and_apply apply matrix

(**********************************
***       Rule eqrr Phase 2     ***
***********************************)

let apply_function_eqrr_phase_2 var recipe =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if not (Recipe.is_free_variable var)
    then Debug.internal_error "[rule.ml >> apply_function_eqrr_phase_2] The variable should be free"
  );
  (***[END DEBUG]***)

  Debug.high_debugging (fun () ->
    if Recipe.get_support var < Recipe.get_max_param_context (Recipe.context_of_recipe recipe)
    then Debug.internal_error "[rule.ml >> apply_function_eqrr_phase_2] The max support of the recipe should be inferior to the support of the variable";

    let rec check_recipe r =
      if Recipe.is_function r
      then
        if Term.is_constructor (Recipe.top r)
        then Recipe.iter_args check_recipe r
        else raise Not_found
      else if Recipe.is_axiom r
      then raise Not_found
      else if Recipe.is_free_variable2 r
      then ()
      else raise Not_found
    in

    try check_recipe recipe with
      | Not_found -> Debug.internal_error "[rule.ml >> apply_function_eqrr_phase_2] The recipe should be only be a constructor term with free variables and no axiom"
  );
  (***[END DEBUG]***)

  let f_test cons = Recipe.is_equal_variable var (Constraint.Deducibility.get_recipe_variable cons) in
  let support = Recipe.get_support var in
  let subst_r = Recipe.create_substitution var recipe in

  let f_after original_csys cons left_csys =
    let message_recipe = Constraint_system.Phase_2.term_of_recipe original_csys recipe in
    let message_var = Constraint.Deducibility.get_message cons in
    let m_var = Term.variable_of_term message_var in

    let subst = Term.create_substitution m_var message_recipe in

    let left_csys_1 = Constraint_system.Phase_2.apply_message_substitution left_csys subst in
    let left_csys_2 = Constraint_system.Phase_2.apply_recipe_substitution left_csys_1 subst_r in

    let right_csys = Constraint_system.Phase_2.add_message_inequation original_csys m_var message_recipe var recipe in

    if Constraint_system.is_unsatisfiable left_csys_2
    then Constraint_system.bottom, right_csys
    else left_csys_2, right_csys
  in

  let search_and_apply csys =
    let cons,pos_cons,left_csys = Constraint_system.Phase_2.deducibility_search_and_replace csys (Constraint.SUnique support) f_test (fun _ -> [])  in
    let left_csys', right_csys' = f_after csys cons left_csys in
    pos_cons, left_csys', right_csys';

  and apply pos_cons csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom, Constraint_system.bottom
    else
      let cons, left_csys = Constraint_system.Phase_2.deducibility_replace csys pos_cons (fun _ -> []) in
      f_after csys cons left_csys
  in

  search_and_apply, apply

let apply_external_eqrr_phase_2 var recipe matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix;
  );
  (***[END DEBUG]***)

  let search_and_apply, apply = apply_function_eqrr_phase_2 var recipe in
  Constraint_system.apply_external_rule search_and_apply apply matrix

(**********************************
***       Rule dedsubterm       ***
***********************************)

let apply_function_dedsubterm support_frame_elt path support_rule f_symbol recipe_sons =
  (* Create the frame *)
  let f_apply frame_elt =
    [Constraint.Frame.add_yesDedSubterm frame_elt f_symbol support_rule],[Constraint.Frame.add_noDedSubterm frame_elt f_symbol support_rule]
  in

  let f_test frame_elt =
    if Recipe.is_path_of_recipe (Constraint.Frame.get_recipe frame_elt) path
    then
      if Constraint.Frame.is_noUse frame_elt
      then raise Constraint_system.Not_applicable
      else true
     else false in

  let f_after frame_elt left_csys right_csys =
    (* The left constraint system *)

    let left_csys' =
      let message = Constraint.Frame.get_message frame_elt in

      if Term.is_function message && Term.is_equal_symbol (Term.top message) f_symbol
      then
        let list_sons = Term.get_args message in
        let list_cons = List.map2 (fun r m -> Constraint.Deducibility.create r support_rule m) recipe_sons list_sons in

        Constraint_system.add_deducibility_constraint left_csys list_cons
      else
        Constraint_system.bottom
    in

    (* The right constraint system *)

    left_csys', right_csys
  in

  let search_and_apply csys =
    try
      let frame_elt, pos_frame, left_csys, right_csys = Constraint_system.frame_search_and_replace2 csys (Constraint.SUnique support_frame_elt) f_test f_apply in

      let left_csys', right_csys' = f_after frame_elt left_csys right_csys in
      pos_frame, left_csys', right_csys'
    with
      Not_found -> raise Constraint_system.Not_applicable

  and apply pos_frame csys =
    if Constraint_system.is_bottom csys
    then Constraint_system.bottom, Constraint_system.bottom
    else
      let frame_elt, left_csys, right_csys = Constraint_system.frame_replace2 csys pos_frame f_apply in
      f_after frame_elt left_csys right_csys
  in

  search_and_apply, apply

let apply_dedsubterm_row_matrix path support symbol support_rule row =

  let arity = Term.get_arity symbol in
  let recipe_sons = Recipe.fresh_variable_list arity support_rule in

  let search_and_apply, apply = apply_function_dedsubterm support path support_rule symbol recipe_sons in
  Constraint_system.apply_rule_on_row_matrix search_and_apply apply row
