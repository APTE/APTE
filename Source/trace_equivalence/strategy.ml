open Standard_library

(********************************************
***     Step a Phase 1 of the strategy    ***
*********************************************)

exception Dest_Found of Recipe.path * int * Term.symbol

let is_dest_applicable support matrix =

  let is_applicable csys =
    if not (Constraint_system.is_bottom csys)
    then
      try
        let frame_elt, _ =
          Constraint.search (Constraint.SUntil support) (fun fc ->
            if Constraint.Frame.is_noUse fc
              || Constraint.Frame.is_yesDest fc
              || Constraint.Frame.is_noDest fc support
              || not (Term.is_function (Constraint.Frame.get_message fc))
            then false
            else
              let symbol = Term.top (Constraint.Frame.get_message fc) in
              List.exists (Term.is_equal_symbol symbol) (Term.senc::Term.aenc::Term.sign::!Term.all_tuple)
          ) (Constraint_system.get_frame csys)
        in

        raise (Dest_Found (
          Recipe.path_of_recipe (Constraint.Frame.get_recipe frame_elt),
          Constraint.Frame.get_support frame_elt,
          Term.top (Constraint.Frame.get_message frame_elt)
        ))
      with
        Not_found -> ()
  in

  try
    Constraint_system.Matrix.iter is_applicable matrix;
    None
  with
    Dest_Found(path,supp_frame,symbol) -> Some(path,supp_frame,symbol)

let is_eqlr_applicable support matrix =
  let eqlr_app = ref [] in

  let is_applicable csys =
    if not (Constraint_system.is_bottom csys)
    then
      Constraint.iter (Constraint.SUnique support) (fun fc ->
        let message = Constraint.Frame.get_message fc in
        if not (Constraint.Frame.is_noUse fc) && Term.is_variable message
        then
          let cons , _ = Constraint.search
            (Constraint.SUntil (support-1))
            (fun dc -> Term.is_equal_term (Constraint.Deducibility.get_message dc) message)
            (Constraint_system.get_deducibility_constraint_set csys)
          in

          eqlr_app := (Recipe.path_of_recipe (Constraint.Frame.get_recipe fc),
            Constraint.Deducibility.get_recipe_variable cons
          ):: !eqlr_app
      ) (Constraint_system.get_frame csys)
  in

  Constraint_system.Matrix.iter is_applicable matrix;

  !eqlr_app

let apply_step_a_phase_1 support matrix =

  let rec apply_several_eqlr list_eqlr_app matrix = match list_eqlr_app with
    | [] -> matrix
    | (path,var)::q ->
        let matrix' = Rules.apply_full_column_eqlr support path var matrix in

        apply_several_eqlr q matrix'
  in

  let get_old_path_support matrix =
    let csys,_ = Constraint_system.Matrix.find_in_row 1 (fun c -> not (Constraint_system.is_bottom c)) matrix in
    Constraint.fold_left (Constraint.SUntil (support-1)) (fun acc fc ->
      (Constraint.Frame.get_support fc, Recipe.path_of_recipe (Constraint.Frame.get_recipe fc))::acc
    ) [] (Constraint_system.get_frame csys)
  in

  let apply_several_eqlr_frame old_path_supp path symbol matrix =
    let path_list =
      if Term.is_tuple symbol
      then
        let proj_list = Term.get_projections symbol in
        List.map (fun s -> Recipe.apply_function_to_path s path) proj_list
       else [Recipe.apply_function_to_path (Term.constructor_to_destructor symbol) path]
    in

    List.fold_left (fun m_1 path_1 ->
      List.fold_left (fun m_2 (s_2,path_2) ->
        Rules.apply_full_column_eqlr_frame support path_1 s_2 path_2 m_2
      ) m_1 old_path_supp
    ) matrix path_list
  in


  let rec apply_dest old_path_supp matrix = match is_dest_applicable support matrix with
    | None -> matrix
    | Some(path,supp_frame,symbol) ->
        let matrix_1, list_eqlr_app =
          if Term.is_tuple symbol
          then
            begin
              (***[BEGIN DEBUG]***)
              Debug.low_debugging (fun () ->
                if support <> supp_frame
                then Debug.internal_error "[strategy.ml >> apply_step_a_phase_1] The support of the frame constraint must be the same as the current support for tuple."
              );
              (***[END DEBUG]***)

              Rules.apply_full_column_dest_tuple supp_frame path symbol matrix
            end
          else Rules.apply_full_column_dest supp_frame path support (Term.constructor_to_destructor symbol) matrix
        in

        (* Application of the eqlr *)
        let matrix_2 = apply_several_eqlr list_eqlr_app matrix_1 in

        (* Application of the eqlr on the frame *)
        let matrix_3 = apply_several_eqlr_frame old_path_supp path symbol matrix_2 in


        (* Recurrence *)
        apply_dest old_path_supp matrix_3
  in

  if Constraint_system.Matrix.is_empty matrix
  then matrix
  else

    let old_path_support = get_old_path_support matrix in

    let matrix_1 = apply_several_eqlr (is_eqlr_applicable support matrix) matrix in

    let matrix_2 =
      List.fold_left (fun m (s,path) ->
        Rules.apply_full_column_eqlr_frame support (Recipe.axiom_path (Recipe.axiom support)) s path m
      ) matrix_1 old_path_support
    in

    apply_dest old_path_support matrix_2

(************************************************
***     Functions for Step b/c of  Phase 1    ***
*************************************************)

let get_list_axiom_cons_applicable_from_eqrr frame cons1 cons2 =
  (***[BEGIN DEBUG]***)
  Debug.low_debugging (fun () ->
    if (Constraint.Deducibility.get_support cons1) < (Constraint.Deducibility.get_support cons2)
    then Debug.internal_error "[strategy.ml >> get_list_axiom_cons_applicable_from_eqrr] The second support should be inferior to the first one";

    if not (Recipe.is_free_variable (Constraint.Deducibility.get_recipe_variable cons2))
    then Debug.internal_error "[strategy.ml >> get_list_axiom_cons_applicable_from_eqrr] The second variable should be free"
  );
  (***[END DEBUG]***)

  let support1 = Constraint.Deducibility.get_support cons1
  and support2 = Constraint.Deducibility.get_support cons2 in

  let is_cons_1_free = Recipe.is_free_variable (Constraint.Deducibility.get_recipe_variable cons1) in

  let comp_cons1,comp_cons2 = Constraint.Deducibility.compare_noCons cons1 cons2
  and comp_ax1,comp_ax2 = Constraint.Deducibility.compare_noAxiom cons1 cons2  (min support1 support2) in

  if support1 = support2
  then
    (* Must have everything the same *)
    let external_cons = List.map (fun f -> Constraint.Deducibility.get_recipe_variable cons2,f) comp_cons2 in
    let external_axiom = List.map (fun pos -> (support2,Constraint.Deducibility.get_recipe_variable cons2, Recipe.path_of_recipe (Constraint.Frame.get_recipe (Constraint.get pos frame)))) comp_ax2 in

    if is_cons_1_free
    then
      let external_cons' = List.fold_left (fun acc f -> (Constraint.Deducibility.get_recipe_variable cons1,f)::acc) external_cons comp_cons1 in
      let external_axiom' = List.fold_left (fun acc pos -> (support2,Constraint.Deducibility.get_recipe_variable cons1,Recipe.path_of_recipe (Constraint.Frame.get_recipe (Constraint.get pos frame)))::acc) external_axiom comp_ax1 in
      (external_cons',external_axiom',[],[])
    else
      let internal_cons = List.map (fun f -> Constraint.Deducibility.get_recipe_variable cons1,f) comp_cons1 in
      let internal_axiom = List.map (fun pos -> (support1,Constraint.Deducibility.get_recipe_variable cons1, Recipe.path_of_recipe (Constraint.Frame.get_recipe (Constraint.get pos frame)))) comp_ax1 in
      (external_cons,external_axiom,internal_cons,internal_axiom)
  else
    let list_cons = List.map (fun f -> (Constraint.Deducibility.get_recipe_variable cons2,f)) comp_cons2 in
    let list_ax =
      List.map (fun pos ->
        ( support2,
          Constraint.Deducibility.get_recipe_variable cons2,
          Recipe.path_of_recipe (Constraint.Frame.get_recipe (Constraint.get pos frame))
        )
      ) comp_ax2 in

    (list_cons,list_ax,[],[])

(******* Application of the rule Axiom on a list of possible application *******)

let get_axiom_apps cur_axiom_apps frame dc =
  let var_dc = Constraint.Deducibility.get_recipe_variable dc in

  Constraint.Deducibility.fold_left_frame_free_of_noAxiom dc (fun cur_axiom_apps_1 fc ->
    if Constraint.Frame.is_noUse fc
      || (Recipe.occurs var_dc (Constraint.Frame.get_recipe fc))
      || not (Term.is_unifiable [Constraint.Frame.get_message fc, Constraint.Deducibility.get_message dc])
    then cur_axiom_apps_1
    else
      let supp = Constraint.Frame.get_support fc in
      let path = Recipe.path_of_recipe (Constraint.Frame.get_recipe fc) in

      if List.exists (fun (s,v,p) -> s = supp && Recipe.is_equal_variable v var_dc && Recipe.is_equal_path p path) cur_axiom_apps_1
      then cur_axiom_apps_1
      else (supp,var_dc,path)::cur_axiom_apps_1
  ) cur_axiom_apps frame

let apply_axiom_row_matrix f_next_left f_next_end axiom_apps row =

  let rec apply_axiom row = function
    | [] -> f_next_end row
    | (s,v,p)::q ->
        begin match Rules.apply_axiom_row_matrix s v p row with
          | None, None -> ()
          | Some(row_l), None -> f_next_left row_l
          | None, Some(row_r) -> apply_axiom row_r q
          | Some(row_l), Some(row_r) ->
              f_next_left row_l;
              apply_axiom row_r q
        end
  in

  apply_axiom row axiom_apps

let rec apply_external_axiom_phase_1 f_next_left f_next_end axiom_apps matrix = match axiom_apps with
  | [] -> f_next_end matrix
  | (s,v,p)::q ->
      let left_matrix,right_matrix = Rules.apply_external_axiom_phase_1 s v p matrix in
      f_next_left left_matrix;
      apply_external_axiom_phase_1 f_next_left f_next_end q right_matrix

(******* Application of the rule Cons on a list of possible application *******)

let apply_cons_row_matrix f_next_left f_next_end cons_apps row =

  let rec apply_cons row = function
    | [] -> f_next_end row
    | (v,f)::q ->
        begin match Rules.apply_cons_row_matrix v f row with
          | None,None -> ()
          | Some(row_l),None -> f_next_left row_l
          | None,Some(row_r) -> apply_cons row_r q
          | Some(row_l), Some(row_r) ->
              f_next_left row_l;
              apply_cons row_r q
        end
  in

  apply_cons row cons_apps

let rec apply_external_cons_phase_1 f_next_left f_next_end cons_apps matrix = match cons_apps with
  | [] -> f_next_end matrix
  | (v,f)::q ->
      let left_matrix,right_matrix = Rules.apply_external_cons_phase_1 v f matrix in
      f_next_left left_matrix;
      apply_external_cons_phase_1 f_next_left f_next_end q right_matrix

(******* Application of the rule Eqrr *******)

let apply_eqrr_row_matrix f_next_ax_cons f_next_eqrr_left f_next_eqrr_right (cons_apps, axiom_apps) var1 var2 row =

  let apply_eqrr row = match Rules.apply_eqrr_row_matrix var1 var2 row with
    | None, None -> ()
    | Some(row_l), None -> f_next_eqrr_left row_l
    | None, Some(row_r) -> f_next_eqrr_right row_r
    | Some(row_l),Some(row_r) ->
        f_next_eqrr_left row_l;
        f_next_eqrr_right row_r
  in

  apply_cons_row_matrix f_next_ax_cons (fun row_1 ->
    apply_axiom_row_matrix f_next_ax_cons (fun row_2 ->
      apply_eqrr row_2
    ) axiom_apps row_1
  ) cons_apps row

let apply_external_eqrr_phase_1 f_next_ax_cons f_next_eqrr_left f_next_eqrr_right (cons_apps, axiom_apps) var1 var2 matrix =

  apply_external_cons_phase_1 f_next_ax_cons (fun matrix_1 ->
    apply_external_axiom_phase_1 f_next_ax_cons (fun matrix_2 ->
      let left_matrix,right_matrix = Rules.apply_external_eqrr_phase_1 var1 var2 matrix_2 in
      f_next_eqrr_left left_matrix;
      f_next_eqrr_right right_matrix
    ) axiom_apps matrix_1
  ) cons_apps matrix

let search_and_apply_internal_eqrr support k_column f_next_external f_next_internal matrix =

  let modification = ref false in
  let found_external_apps = ref None in

  let apply_on_row row = match !found_external_apps with
    | Some _ -> [row]
    | None ->
        let csys = Constraint_system.Row.get row k_column in

        if Constraint_system.is_bottom csys
        then [row]
        else begin
          let dc_set = Constraint_system.get_deducibility_constraint_set csys in

          let new_row_list = ref [] in

          let list_apps =
            Constraint.fold_left (Constraint.SUnique support) (fun acc dc ->
              let var_dc = Constraint.Deducibility.get_recipe_variable dc in
              let mess_dc = Constraint.Deducibility.get_message dc in

              if not (Recipe.is_free_variable var_dc) && Term.is_variable mess_dc
              then
                try
                  let dc_free,_ =
                    Constraint.search (Constraint.SUntil (support-1)) (fun dc' ->
                      Term.is_equal_term mess_dc (Constraint.Deducibility.get_message dc')
                    ) dc_set
                  in

                  let ex_cons,ex_ax,in_cons,in_ax = get_list_axiom_cons_applicable_from_eqrr (Constraint_system.get_frame csys) dc dc_free in

                  if (ex_cons <> [] || ex_ax <> []) && !found_external_apps = None
                  then (found_external_apps := Some(ex_cons,ex_ax); acc)
                  else (in_cons,in_ax,var_dc,(Constraint.Deducibility.get_recipe_variable dc_free))::acc
                with Not_found -> acc
              else acc

            ) [] dc_set
          in

          if list_apps <> []
          then modification := true;

          let add_row row = new_row_list := row :: !new_row_list in

          let rec apply_list_apps list_apps row = match list_apps with
            | [] -> new_row_list := row :: !new_row_list
            | (cons_apps,ax_apps,var1,var2)::q ->
                apply_eqrr_row_matrix add_row (apply_list_apps q) add_row (cons_apps, ax_apps) var1 var2 row
          in

          apply_list_apps list_apps row;

          !new_row_list
        end

  in

  let matrix' = Constraint_system.Matrix.replace_row apply_on_row matrix in

  match !found_external_apps,!modification with
    | None,false -> raise Not_found
    | None,_ -> f_next_internal matrix'
    | Some(cons_apps,axiom_apps), _ ->
        apply_external_cons_phase_1 f_next_external (fun matrix_1 ->
          apply_external_axiom_phase_1 f_next_external f_next_external axiom_apps matrix_1
        ) cons_apps matrix


(************************************************
***            Step b of  Phase 1             ***
*************************************************)

(* In this step, we apply the internal rules Eqrr, Eqll, DedSubterm, Cons, Axiom.
   Since after step a, no new frame constraint can be added in the frame, we may
   apply eqll and dedsubterm directly on the all matrix before starting to work
   on each column at a time. *)

let search_and_apply_dedsubterm support column matrix =
  let modification = ref false in

  let apply_on_row row =
    let get_dedsubterm_app csys =
      if Constraint_system.is_bottom csys
      then raise Not_found
      else
        let fc,_ =
          Constraint.search (Constraint.SUntil support) (fun fc ->
            let message = Constraint.Frame.get_message fc in

            if not (Constraint.Frame.is_noUse fc) && Term.is_function message
            then
              let symbol = Term.top message in
              Term.is_constructor symbol
                && not (Term.is_tuple symbol)
                && not (Term.is_equal_symbol Term.senc symbol)
                && not (Constraint.Frame.is_noDedSubterm fc support)
                && not (Constraint.Frame.is_yesDedSubterm fc support)
            else false
          ) (Constraint_system.get_frame csys)
        in

        let symbol =  Term.top (Constraint.Frame.get_message fc)
        and path = Recipe.path_of_recipe (Constraint.Frame.get_recipe fc)
        and supp_fc = Constraint.Frame.get_support fc in

        path, supp_fc, symbol
    in

    let csys = Constraint_system.Row.get row column in

    try
      let path,supp,symbol = get_dedsubterm_app csys in
      modification := true;
      begin match Rules.apply_dedsubterm_row_matrix path supp symbol support row with
        | None,None -> []
        | Some(row_l), None -> [row_l]
        | None, Some(row_r) -> [row_r]
        | Some(row_l), Some(row_r) -> [row_l;row_r]
      end
    with
      Not_found -> [row]
  in

  let matrix' = Constraint_system.Matrix.replace_row apply_on_row matrix in

  if !modification
  then matrix'
  else raise Not_found

let apply_pre_cycle_b_c support matrix =
  Rules.apply_eqll support matrix

let rec apply_step_b_phase_1 support column_k f_next matrix =

  let modification = ref false in

  let apply_on_row row =
    let new_row_list = ref [] in

    let get_list_apps csys =

      let frame = Constraint_system.get_frame csys in
      let dc_set = Constraint_system.get_deducibility_constraint_set csys in

      Constraint.fold_left (Constraint.SUnique support) (fun (cons_apps,axiom_apps) dc ->
        let var_dc = Constraint.Deducibility.get_recipe_variable dc in
        let mess_dc = Constraint.Deducibility.get_message dc in

        if Recipe.is_free_variable var_dc
        then (cons_apps,axiom_apps)
        else if Term.is_variable mess_dc
        then
          (* In case of there is a variable, nothing is applied unless all
             constructor are already applied. *)
          if Constraint.Deducibility.is_all_noCons dc
          then (cons_apps,get_axiom_apps axiom_apps frame dc)
          else (cons_apps,axiom_apps)
        else if Term.is_function mess_dc
        then
          let symbol = Term.top mess_dc in
          if Constraint.Deducibility.is_noCons dc symbol
          then (cons_apps,get_axiom_apps axiom_apps frame dc)
          else (var_dc,symbol)::cons_apps,get_axiom_apps axiom_apps frame dc
        else cons_apps, get_axiom_apps axiom_apps frame dc
      ) ([],[]) dc_set
    in

    let add_row row = new_row_list := row :: !new_row_list in

    let rec apply_cons_axiom row =
      let csys = Constraint_system.Row.get row column_k in

      if Constraint_system.is_bottom csys
      then add_row row
      else
        let cons_apps,axiom_apps = get_list_apps csys in

        if cons_apps <> [] || axiom_apps <> []
        then
          begin
            modification := true;
            apply_cons_row_matrix add_row (fun row_1 ->
              apply_axiom_row_matrix apply_cons_axiom add_row axiom_apps row_1
            ) cons_apps row
          end
        else add_row row
    in

    apply_cons_axiom row;

    !new_row_list
  in

  try
    search_and_apply_internal_eqrr support column_k
      (apply_step_b_phase_1 support column_k f_next) (fun matrix_1 ->
      let matrix_1' = Constraint_system.Matrix.replace_row apply_on_row matrix_1 in
      apply_step_b_phase_1 support column_k f_next matrix_1') matrix
  with
    Not_found ->
      let matrix' = Constraint_system.Matrix.replace_row apply_on_row matrix in

      if !modification
      then apply_step_b_phase_1 support column_k f_next matrix'
      else
        begin
          try
            let matrix'' = search_and_apply_dedsubterm support column_k matrix in
            let matrix''' = Constraint_system.Matrix.normalise matrix'' in
            apply_step_b_phase_1 support column_k f_next matrix'''
          with Not_found ->
            f_next matrix
        end

(************************************************
***            Step c of  Phase 1             ***
*************************************************)

exception No_rule_applied_step_c

let add_in_list_apps frame dc list_apps =

  let var_dc = Constraint.Deducibility.get_recipe_variable dc in
  let support = Recipe.get_support var_dc in
  let symbol = Term.top (Constraint.Deducibility.get_message dc) in

  (* Add a test to confirm that AXIOM is always applied by increasing support. *)

  let rec go_through = function
    | [] ->
        if Constraint.Deducibility.is_noCons dc symbol
        then [var_dc, [], get_axiom_apps [] frame dc]
        else [var_dc, [symbol], get_axiom_apps [] frame dc]
    | (v,f_l,ax)::q when Recipe.is_equal_variable v var_dc ->
        if Constraint.Deducibility.is_noCons dc symbol || List.exists (Term.is_equal_symbol symbol) f_l
        then (v,f_l,get_axiom_apps ax frame dc)::q
        else (v,symbol::f_l,get_axiom_apps ax frame dc)::q
    | (((v,_,_)::_) as l) when support < Recipe.get_support v ->
        if Constraint.Deducibility.is_noCons dc symbol
        then (var_dc, [], get_axiom_apps [] frame dc)::l
        else (var_dc, [symbol], get_axiom_apps [] frame dc)::l
    | t::q -> t::(go_through q)
  in

  go_through list_apps

let apply_step_c_phase_1 support column_k function_next matrix =
  let get_axiom_cons_list_apps matrix =
    Constraint_system.Matrix.fold_left_on_column column_k (fun acc_1 csys ->
      if Constraint_system.is_bottom csys
      then acc_1
      else

        let list_not_free_m_var =
          Constraint.fold_left (Constraint.SUnique support) (fun acc_var dc ->
            let var_dc = Constraint.Deducibility.get_recipe_variable dc in
            let mess_dc = Constraint.Deducibility.get_message dc in

            if not (Recipe.is_free_variable var_dc) && Term.is_variable mess_dc
            then (Term.variable_of_term mess_dc)::acc_var
            else acc_var

          ) [] (Constraint_system.get_deducibility_constraint_set csys)
        in

        if list_not_free_m_var = []
        then acc_1
        else

          Constraint.fold_left (Constraint.SUntil (support-1)) (fun acc_2 dc ->
            if Term.var_occurs_list list_not_free_m_var (Constraint.Deducibility.get_message dc)
            then add_in_list_apps (Constraint_system.get_frame csys) dc acc_2
            else acc_2
          ) acc_1 (Constraint_system.get_deducibility_constraint_set csys)
    ) [] matrix
  in

  let rec go_through_cons_ax list_apps matrix = match list_apps with
    | [] ->
        (***[BEGIN DEBUG]***)
        Debug.high_debugging (fun () ->
          try
            search_and_apply_internal_eqrr support column_k (fun _ -> ()) (fun _ -> ()) matrix;
            Debug.internal_error "[strategy.ml >>  apply_step_c_phase_1] No rule Eqrr internal should be applicable at this point (1)";
          with Not_found -> ();

          if get_axiom_cons_list_apps matrix <> []
          then Debug.internal_error "[strategy.ml >>  apply_step_c_phase_1] No rule Axiom or Cons external should be applicable at this point";
        );
        (***[END DEBUG]***)

        (***[Statistic]***)
        Statistic.record_matrix Statistic.POne_SC matrix;

        function_next matrix
    | (v,f_l,ax_apps)::q ->

        apply_external_axiom_phase_1 (fun matrix' ->
          (***[Statistic]***)
          Statistic.record_matrix Statistic.POne_SC matrix';

          (***[BEGIN DEBUG]***)
          Debug.high_debugging (fun () ->
            try
              search_and_apply_internal_eqrr support column_k (fun _ -> ()) (fun _ -> ()) matrix';
              Debug.internal_error "[strategy.ml >>  apply_step_c_phase_1] No rule Eqrr internal shoudl be applicable at this point (2)";
            with Not_found -> ()
          );
          (***[END DEBUG]***)

          go_through_cons_ax q matrix'
        ) (apply_external_cons_phase_1 go_through_eqrr (go_through_cons_ax q) (List.map (fun symbol -> (v,symbol)) f_l)
        ) ax_apps matrix

  and go_through_eqrr matrix =
    (***[Statistic]***)
    Statistic.record_matrix Statistic.POne_SC matrix;

    try
      search_and_apply_internal_eqrr support column_k
        (apply_step_b_phase_1 support column_k go_through_all)
        go_through_all
        matrix
    with
      Not_found -> go_through_all matrix

  and go_through_all matrix =
    (***[Statistic]***)
    Statistic.record_matrix Statistic.POne_SC matrix;

    let list_apps = get_axiom_cons_list_apps matrix in
    if list_apps = []
    then function_next matrix
    else go_through_cons_ax list_apps matrix

  in

  if Constraint_system.Matrix.is_empty matrix
  then raise No_rule_applied_step_c
  else
    let list_apps = get_axiom_cons_list_apps matrix in
    if list_apps = []
    then raise No_rule_applied_step_c
    else go_through_cons_ax list_apps matrix

(********************************************
***     Step d Phase 1 of the strategy    ***
*********************************************)

let rec apply_step_d_phase_1 support column function_next matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Invariant.invPPunSb support (column-1) matrix
  );
  (***[END DEBUG]***)

  let (eqrr_apps,axiom_apps,cons_apps), matrix' =
    let axiom_apps = ref []
    and cons_apps = ref []
    and eqrr_apps = ref [] in

    let map_csys csys =
      if Constraint_system.is_bottom csys
        || Constraint_system.is_semi_solved_form csys
        || !axiom_apps <> []
        || !cons_apps <> []
        || !eqrr_apps <> []
      then csys
      else begin
        let dc_set = Constraint_system.get_deducibility_constraint_set csys in

        Constraint.iter (Constraint.SUntil support) (fun dc ->
          let mess_dc = Constraint.Deducibility.get_message dc in
          let var_dc = Constraint.Deducibility.get_recipe_variable dc in
          let supp_dc = Recipe.get_support var_dc in

          if Term.is_variable mess_dc
          then
            (* Check for EQRR application *)
            let dc',_ = Constraint.search (Constraint.SUntil supp_dc) (fun dc' -> Term.is_equal_term mess_dc (Constraint.Deducibility.get_message dc')) dc_set in

            if Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable dc') var_dc
            then
              (* Not Eqrr applicable *)

              if Constraint.Deducibility.is_all_noCons dc
              then axiom_apps := get_axiom_apps !axiom_apps (Constraint_system.get_frame csys) dc
              else ()
            else
              let ex_c_l,ex_ax_l,in_c_l,in_ax_l = get_list_axiom_cons_applicable_from_eqrr (Constraint_system.get_frame csys) dc dc' in

              if in_c_l <> [] || in_ax_l <> []
              then Debug.internal_error "[strategy.ml >>  apply_step_d_phase_1] No internal rule should be detected"
              else eqrr_apps := (var_dc,Constraint.Deducibility.get_recipe_variable dc',(ex_c_l,ex_ax_l)) :: !eqrr_apps

          else if Term.is_function mess_dc
          then
            begin
              let symbol = Term.top mess_dc in

              if not (Constraint.Deducibility.is_noCons dc symbol)
              then cons_apps := (var_dc,symbol) :: !cons_apps;

              axiom_apps := get_axiom_apps !axiom_apps (Constraint_system.get_frame csys) dc
            end
          else axiom_apps := get_axiom_apps !axiom_apps (Constraint_system.get_frame csys) dc
        ) dc_set;

        if !axiom_apps = [] && !cons_apps = [] && !eqrr_apps = []
        then Constraint_system.set_semi_solved_form csys
        else csys
      end
    in


    let matrix' = Constraint_system.Matrix.map_on_column column map_csys matrix in

    (!eqrr_apps,!axiom_apps,!cons_apps), matrix'
  in

  let rec apply_all list_apps matrix = match list_apps with
    | [] ->
        apply_external_axiom_phase_1 (apply_step_d_phase_1 support column function_next) (
          apply_external_cons_phase_1 (apply_step_d_phase_1 support column function_next) (
            apply_step_d_phase_1 support column function_next
          ) cons_apps
        ) axiom_apps matrix
    | (v1,v2,cons_ax_apps)::q ->
        apply_external_eqrr_phase_1
          (apply_step_d_phase_1 support column function_next)
          (apply_all q)
          (apply_step_d_phase_1 support column function_next)
          cons_ax_apps v1 v2 matrix
  in


  if axiom_apps = [] && cons_apps = [] && eqrr_apps = []
  then function_next matrix'
  else apply_all eqrr_apps matrix'

(********************************************
***     Step e Phase 1 of the strategy    ***
*********************************************)

type result_compare =
  | Keep
  | Keep_and_delete
  | Delete

exception Result_step_e of result_compare

let compare_frame_constraint_of_same_path support fc_prev fc =

  if Constraint.Frame.is_yesDedSubterm fc_prev support && Constraint.Frame.is_noDedSubterm fc support
  then Delete
  else if Constraint.Frame.is_noDedSubterm fc_prev support && Constraint.Frame.is_yesDedSubterm fc support
  then Keep_and_delete
  else if Constraint.Frame.is_yesDest fc_prev && Constraint.Frame.is_noDest fc support
  then Delete
  else if Constraint.Frame.is_noDest fc_prev support && Constraint.Frame.is_yesDest fc
  then Keep_and_delete
  else Keep

let compare_frame support frame_prev frame =
  try
    Constraint.iter2 (Constraint.SUntil (support-1)) (fun fc_prev fc ->
        match compare_frame_constraint_of_same_path support fc_prev fc with
          | Keep -> ()
          | r -> raise (Result_step_e r)
    ) frame_prev frame;

    Constraint.iter (Constraint.SUnique support) (fun fc_prev ->
      try
        Constraint.iter (Constraint.SUnique support) (fun fc ->
          if Recipe.is_recipe_same_path (Constraint.Frame.get_recipe fc_prev) (Constraint.Frame.get_recipe fc)
          then raise (Result_step_e (compare_frame_constraint_of_same_path support fc_prev fc))
        ) frame;
        raise (Result_step_e Delete)
      with
        | Result_step_e Keep -> ()
    ) frame_prev;

    Keep
  with
    | Result_step_e r -> r

let apply_step_e_phase_1 support matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.check_structure matrix;
  );

  let number_column = Constraint_system.Matrix.get_number_column matrix in

  let prev_frames = Array.make number_column None in

  Constraint_system.Matrix.fold_right_row (fun row prev_matrix ->
    let list_keep_and_delete = ref [] in
    let all_bottom = ref true in

    let _, new_csys_list =
      Constraint_system.Row.fold_right (fun csys (k,csys_list) ->
        if Constraint_system.is_bottom csys
        then (k-1,csys::csys_list)
        else
          match prev_frames.(k) with
            | None ->
                prev_frames.(k) <- Some(Constraint_system.get_frame csys);
                all_bottom := false;
                (k-1,csys::csys_list)
            | Some(frame_prev) ->
                let frame = Constraint_system.get_frame csys in

                begin match compare_frame support frame_prev frame with
                  | Keep ->
                      prev_frames.(k) <- Some(frame);
                      all_bottom := false;
                      (k-1,csys::csys_list)
                  | Delete ->
                      (k-1,Constraint_system.bottom::csys_list)
                  | Keep_and_delete ->
                      prev_frames.(k) <- Some(frame);
                      all_bottom := false;
                      list_keep_and_delete := (k+1) :: !list_keep_and_delete;
                      (k-1,csys::csys_list)
                end
      ) row (number_column-1,[])
    in

    let prev_matrix' =
      List.fold_left (fun prev_matrix_1 k ->
        Constraint_system.Matrix.map_on_column k (fun _ -> Constraint_system.bottom) prev_matrix_1
      ) prev_matrix !list_keep_and_delete in

    if !all_bottom
    then prev_matrix'
    else Constraint_system.Matrix.add_row prev_matrix' (Constraint_system.Row.create number_column new_csys_list)
  ) matrix Constraint_system.Matrix.empty

(*************************************
***     Phase 1 of the strategy    ***
**************************************)

let rec apply_cycle_b_c_phase_1 support column function_next matrix =
  apply_step_b_phase_1 support column (fun matrix' ->
    (***[Statistic]***)
    Statistic.record_matrix Statistic.POne_SB matrix';

    try
      apply_step_c_phase_1 support column (fun matrix'' ->
        (***[Statistic]***)
        Statistic.record_matrix Statistic.POne_SC matrix'';

        apply_cycle_b_c_phase_1 support column function_next matrix''
      ) matrix'
    with
      No_rule_applied_step_c -> function_next matrix'
  ) matrix

let rec apply_step_b_c_d_phase_1 support column function_next matrix =
  if column > Constraint_system.Matrix.get_number_column matrix
  then function_next matrix
  else
    apply_cycle_b_c_phase_1 support column (fun matrix_1 ->
      apply_step_d_phase_1 support column (fun matrix_2 ->
        (***[Statistic]***)
        Statistic.record_matrix Statistic.POne_SD matrix_2;

        apply_step_b_c_d_phase_1 support (column + 1) function_next matrix_2
      ) matrix_1
    ) matrix

let rec apply_step_d_all_column_phase_1 support column function_next matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Invariant.invPPunSb support (column-1) matrix
  );
  (***[END DEBUG]***)

  if column > Constraint_system.Matrix.get_number_column matrix
  then function_next matrix
  else
    apply_step_d_phase_1 support column (fun matrix_1 ->
      apply_step_d_all_column_phase_1 support (column + 1) function_next matrix_1
    ) matrix

(******* Phase 1 of the strategy after an output ********)

let apply_phase_1_output support function_next matrix =
  apply_step_d_all_column_phase_1 (support-1) 1 (fun matrix_0 ->
    (***[Statistic]***)
    Statistic.record_matrix Statistic.POne_SD matrix_0;

    let matrix_1 = Constraint_system.Matrix.map Constraint_system.unset_semi_solved_form matrix_0 in
    let matrix_2 = apply_step_a_phase_1 support matrix_1 in

    (***[Statistic]***)
    Statistic.record_matrix Statistic.POne_SA matrix_2;

    let matrix_3 = apply_pre_cycle_b_c support matrix_2 in
    let matrix_4 = Constraint_system.Matrix.normalise matrix_3 in
    apply_step_b_c_d_phase_1 support 1 (fun matrix_5 ->
      let matrix_6 = Constraint_system.Matrix.map Constraint_system.unset_semi_solved_form matrix_5 in
      let matrix_7 = apply_step_e_phase_1 support matrix_6 in

      (***[Statistic]***)
      Statistic.record_matrix Statistic.POne_SE matrix_7;

      function_next matrix_7

    ) matrix_4
  ) matrix

(******* Phase 1 of the strategy after an input ********)

let apply_phase_1_input support function_next matrix =
  apply_step_d_all_column_phase_1 support 1 (fun matrix_0 ->
    (***[Statistic]***)
    Statistic.record_matrix Statistic.POne_SD matrix_0;

    function_next (Constraint_system.Matrix.map Constraint_system.unset_semi_solved_form matrix_0)
  ) matrix

(******* Phase 1 of the strategy ********)

let apply_phase_1 function_next matrix =

  let max_support = Constraint_system.Matrix.get_maximal_support matrix in

  let rec apply_each_support support matrix_1 =
    if support > max_support
    then function_next matrix_1
    else
      begin
      let matrix_2 = apply_step_a_phase_1 support matrix_1 in

      (***[Statistic]***)
      Statistic.record_matrix Statistic.POne_SA matrix_2;

      let matrix_3 = apply_pre_cycle_b_c support matrix_2 in
      let matrix_4 = Constraint_system.Matrix.normalise matrix_3 in
      apply_step_b_c_d_phase_1 support 1 (fun matrix_5 ->
        let matrix_6 = Constraint_system.Matrix.map Constraint_system.unset_semi_solved_form matrix_5 in
        let matrix_7 = apply_step_e_phase_1 support matrix_6 in

        (***[Statistic]***)
        Statistic.record_matrix Statistic.POne_SE matrix_7;

        apply_each_support (support + 1) matrix_7
      ) matrix_4
      end
  in

  apply_each_support 1 matrix

(***************************************************
***     Functions for Phase 2 of the strategy    ***
****************************************************)

let rec apply_external_axiom_phase_2 f_next_left f_next_end axiom_apps matrix = match axiom_apps with
  | [] ->
      f_next_end matrix
  | (s,v,p)::q ->
      let left_matrix,right_matrix = Rules.apply_external_axiom_phase_2 s v p matrix in
      f_next_left left_matrix;
      apply_external_axiom_phase_2 f_next_left f_next_end q right_matrix

let rec apply_external_cons_phase_2 f_next_left f_next_end cons_apps matrix = match cons_apps with
  | [] -> f_next_end matrix
  | (v,f)::q ->
      let left_matrix,right_matrix = Rules.apply_external_cons_phase_2 v f matrix in
      f_next_left left_matrix;
      apply_external_cons_phase_2 f_next_left f_next_end q right_matrix

let apply_external_eqrr_phase_2 f_next_ax_cons f_next_eqrr_left f_next_eqrr_right (cons_apps, axiom_apps) var recipe matrix =
  apply_external_cons_phase_2 f_next_ax_cons (fun matrix_1 ->
    apply_external_axiom_phase_2 (fun m ->
      f_next_ax_cons m) (fun matrix_2 ->
      let left_matrix,right_matrix = Rules.apply_external_eqrr_phase_2 var recipe matrix_2 in
      f_next_eqrr_left left_matrix;
      f_next_eqrr_right right_matrix;
    ) axiom_apps matrix_1
  ) cons_apps matrix

(***********************************************
***     Step a of Phase 2 of the strategy    ***
************************************************)

let get_axiom_apps_phase_2 frame dc var_dc var_term ineq_term =

  Constraint.Deducibility.fold_left_frame_free_of_noAxiom dc (fun cur_axiom_apps_1 fc ->
    if Constraint.Frame.is_noUse fc
      || (Recipe.occurs var_dc (Constraint.Frame.get_recipe fc))
      || (Term.var_occurs var_term (Constraint.Frame.get_message fc))
      || not (Term.is_unifiable [Constraint.Frame.get_message fc, ineq_term])
    then cur_axiom_apps_1
    else
      let supp = Constraint.Frame.get_support fc in
      let path = Recipe.path_of_recipe (Constraint.Frame.get_recipe fc) in

      (supp,var_dc,path)::cur_axiom_apps_1
  ) [] frame


let rec apply_step_a_phase_2 function_next matrix =

  let apps, matrix' =
    let application = ref None in

    let map_csys csys =
      if Constraint_system.is_bottom csys
        || Constraint_system.is_no_universal_variable csys
        || !application <> None
      then csys
      else
        let dc_set = Constraint_system.get_deducibility_constraint_set csys
        and frame = Constraint_system.get_frame csys in

        let csys' =
          Constraint_system.map_message_inequation (fun formula ->
            if !application <> None
            then formula
            else Term.find_and_apply_formula
              (fun t1 t2 -> Term.exists_var Term.Universal t2 || Term.exists_var Term.Universal t1)
              (fun t1 t2 ->
                let t_var, t_uni = if Term.is_variable t1 then t1,t2 else t2,t1 in

                let var_dc = Recipe.variable_of_recipe (Constraint_system.Phase_2.recipe_of_term csys t_var) in
                let dc,_ = Constraint.search
                  (Constraint.SUnique (Recipe.get_support var_dc))
                  (fun dc -> Recipe.is_equal_variable var_dc (Constraint.Deducibility.get_recipe_variable dc))
                  dc_set
                in

                let axiom_apps = get_axiom_apps_phase_2 frame dc var_dc (Term.variable_of_term t_var) t_uni in
                let cons_apps =
                  if Constraint.Deducibility.is_noCons dc (Term.top t_uni)
                  then []
                  else [var_dc,(Term.top t_uni)]
                in

                if axiom_apps = [] && cons_apps = []
                then Term.top_formula
                else
                  begin
                    application := Some(axiom_apps,cons_apps);
                    formula
                  end
              )
              (fun () -> formula)
              formula
          ) csys
        in

        if !application = None
        then Constraint_system.set_no_universal_variable csys'
        else csys'
    in

    let matrix' = Constraint_system.Matrix.map map_csys matrix in

    !application,matrix'
  in

  match apps with
    | None -> function_next matrix'
    | Some(axiom_apps,cons_apps) ->
        apply_external_axiom_phase_2 (fun m ->
          apply_step_a_phase_2 function_next m) (fun m ->
          apply_external_cons_phase_2 (apply_step_a_phase_2 function_next) (
            apply_step_a_phase_2 function_next
          ) cons_apps m
        ) axiom_apps matrix'

(********************************************
***     Step b Phase 2 of the strategy    ***
*********************************************)

type application_step_b_phase_2 =
  | No_rule_applicable
  | Eqrr of Recipe.variable * Recipe.recipe
  | Eqrr_var of Recipe.variable * Recipe.recipe * ((Recipe.variable * Term.symbol) list * (int * Recipe.variable * Recipe.path) list)
  | Cons of Recipe.variable * Term.symbol

let check_eqrr_var_phase_2 ref_rule list_rule dc_set frame v1 v2 =
  if List.exists (function
    | Eqrr_var (v,r,_) ->
        let var_r = Recipe.variable_of_recipe r in
        Recipe.is_equal_variable  v1 v
        || Recipe.is_equal_variable v1 var_r
        || Recipe.is_equal_variable v2 v
        || Recipe.is_equal_variable v2 var_r
    | _ -> false
    ) list_rule
  then false
  else
    let dc1,_ = Constraint.search (Constraint.SUnique (Recipe.get_support v1)) (fun dc -> Recipe.is_equal_variable v1 (Constraint.Deducibility.get_recipe_variable dc)) dc_set
    and dc2,_ = Constraint.search (Constraint.SUnique (Recipe.get_support v2)) (fun dc -> Recipe.is_equal_variable v2 (Constraint.Deducibility.get_recipe_variable dc)) dc_set in

    let (ex_cons,ex_axiom,in_cons,in_axiom) = get_list_axiom_cons_applicable_from_eqrr frame dc1 dc2 in

    if in_cons <> [] || in_axiom <> []
    then Debug.internal_error "[check_eqrr_var_phase_2] There should be no internal rule applicable"
    else (ref_rule := Eqrr_var(v1,Recipe.recipe_of_variable v2,(ex_cons,ex_axiom)); true)

let check_cons_phase_2 dc_set ref_rule r_dc symbol =
  let var_dc = Recipe.variable_of_recipe r_dc in
  let dc,_ = Constraint.search
    (Constraint.SUnique (Recipe.get_support var_dc))
    (fun dc -> Recipe.is_equal_variable var_dc (Constraint.Deducibility.get_recipe_variable dc))
    dc_set
  in

  if Constraint.Deducibility.is_noCons dc symbol
  then false
  else (ref_rule := Cons (var_dc,symbol); true)

let check_eqrr_phase_2 dc_set ref_rule r_dc r2 =
  let var_dc = Recipe.variable_of_recipe r_dc in
  let dc,_ = Constraint.search
    (Constraint.SUnique (Recipe.get_support var_dc))
    (fun dc -> Recipe.is_equal_variable var_dc (Constraint.Deducibility.get_recipe_variable dc))
    dc_set
  in
  let symbol = Recipe.top r2 in

  if Constraint.Deducibility.is_noCons dc symbol
  then false
  else (ref_rule := Eqrr (var_dc,r2); true)



let rec remove_rule_from_list rule list_rule = match rule,list_rule with
  | _,[] -> []
  | Eqrr(v1,r1), (Eqrr(v2,r2) as t)::q
  | Eqrr_var(v1,r1,_), (Eqrr_var(v2,r2,_) as t)::q ->
      if Recipe.is_equal_variable v1 v2 && Recipe.is_equal_recipe r1 r2
      then remove_rule_from_list rule q
      else t::(remove_rule_from_list rule q)
  | Cons(v1,f1), (Cons(v2,f2) as t)::q ->
      if Recipe.is_equal_variable v1 v2 && Term.is_equal_symbol f1 f2
      then remove_rule_from_list rule q
      else t::(remove_rule_from_list rule q)
  | _,t::q -> t::(remove_rule_from_list rule q)

let search_next_rule_for_inequation acc_rule csys =
  if Constraint_system.is_bottom csys
  then acc_rule
  else

  let dc_set = Constraint_system.get_deducibility_constraint_set csys in
  let frame = Constraint_system.get_frame csys in

  Constraint_system.Phase_2.fold_left_message_inequation (fun acc_rule m_formula assoc_table ->
    match assoc_table with
      | None ->
          let rule_found = ref No_rule_applicable in

          Term.find_and_apply_formula (fun t1 t2 ->
            if Term.is_variable t1
            then
              if Term.exists_name t2
              then
                (* Only the rule Cons can be applied here *)
                if Term.is_name t2
                then false
                else check_cons_phase_2 dc_set rule_found (Constraint_system.Phase_2.recipe_of_term csys t1) (Term.top t2)
              else
                (* Cons or eqrr is applied can be applied *)
                let r1 = Constraint_system.Phase_2.recipe_of_term csys t1 in
                let r2 = Constraint_system.Phase_2.recipe_of_term csys t2 in
                let v1 = Recipe.variable_of_recipe r1 in

                if Term.is_variable t2
                then
                  let v2 = Recipe.variable_of_recipe r2 in
                  if Recipe.get_support v1 < Recipe.get_support v2
                  then check_eqrr_var_phase_2 rule_found acc_rule dc_set frame v2 v1
                  else check_eqrr_var_phase_2 rule_found acc_rule dc_set frame v1 v2
                else
                  let mpc_t1 = Recipe.get_support v1
                  and mpc_t2 = Constraint_system.Phase_2.get_max_param_context csys r2 in

                  if mpc_t1 < mpc_t2
                  then check_cons_phase_2 dc_set rule_found r1 (Term.top t2)
                  else check_eqrr_phase_2 dc_set rule_found r1 r2
            else
              Debug.internal_error "[strategy.ml >> search_next_rule_for_inequation] This case should not happen"
          ) (fun _ _ -> !rule_found :: acc_rule) (fun _ -> acc_rule) m_formula
      | Some(r_formula) ->
          let potential_rule = ref No_rule_applicable in
          let found_path = ref false in
          let rule_found = ref No_rule_applicable in

          Recipe.find_and_apply_formula (fun c1 c2 -> match !potential_rule with
            | No_rule_applicable ->
                if not (Recipe.is_variable_context c1)
                then (found_path := true; false)
                else
                  if Recipe.is_variable_context c2
                  then
                    let r1 = Recipe.recipe_of_context c1
                    and r2 = Recipe.recipe_of_context c2 in
                    let v_r1 = Recipe.variable_of_recipe r1
                    and v_r2 = Recipe.variable_of_recipe r2 in

                    if !found_path
                    then
                      if (Recipe.get_max_param_context c1) >= (Recipe.get_max_param_context c2)
                      then check_eqrr_var_phase_2 rule_found acc_rule dc_set frame v_r1 v_r2
                      else check_eqrr_var_phase_2 rule_found acc_rule dc_set frame v_r2 v_r1
                    else
                      if (Recipe.get_max_param_context c1) >= (Recipe.get_max_param_context c2)
                      then (ignore (check_eqrr_var_phase_2 potential_rule acc_rule dc_set frame v_r1 v_r2); false)
                      else (ignore (check_eqrr_var_phase_2 potential_rule acc_rule  dc_set frame v_r2 v_r1); false)
                  else if Recipe.exists_path_in_context c2
                  then
                    let r1 = Recipe.recipe_of_context c1 in
                    let symbol =
                      if Recipe.is_path_context c2
                      then
                        let path = Recipe.path_of_context c2 in
                        let fc,_ = Constraint.search Constraint.SAll (fun fc -> Recipe.is_path_of_recipe (Constraint.Frame.get_recipe fc) path) frame in
                        Term.top (Constraint.Frame.get_message fc)
                      else Recipe.top_context c2
                    in
                    (found_path := true; check_cons_phase_2 dc_set rule_found r1 symbol)
                  else
                    let r1 = Recipe.recipe_of_context c1 in
                    let r2 = Recipe.recipe_of_context c2 in

                    if Recipe.get_max_param_context c1 < Recipe.get_max_param_context c2
                    then check_cons_phase_2 dc_set rule_found r1 (Recipe.top r2)
                    else
                      if !found_path
                      then check_eqrr_phase_2 dc_set rule_found r1 r2
                      else let _ = check_eqrr_phase_2 dc_set potential_rule r1 r2 in false
            | pot_rule ->
                if not (Recipe.is_variable_context c1)
                then (rule_found := pot_rule; true)
                else
                  if Recipe.is_variable_context c2
                  then false
                  else if Recipe.exists_path_in_context c2
                  then (rule_found := pot_rule; true)
                  else
                    if Recipe.get_max_param_context c1 < Recipe.get_max_param_context c2
                    then check_cons_phase_2 dc_set rule_found (Recipe.recipe_of_context c1) (Recipe.top_context c2)
                    else false
          ) (fun _ _ -> !rule_found :: acc_rule) (fun _ -> acc_rule) r_formula
  ) acc_rule csys

let rec apply_step_b_phase_2 function_next matrix =

  let rec apply_rule list_app matrix = match list_app with
    | [] -> apply_step_b_phase_2 function_next matrix
    | No_rule_applicable::_ -> Debug.internal_error "[strategy.ml >> apply_step_b_phase_2] Unexpected case"
    | (Cons(var,symbol) as t)::q ->
        let left_matrix,right_matrix = Rules.apply_external_cons_phase_2 var symbol matrix in
        apply_step_b_phase_2 function_next left_matrix;
        apply_rule (remove_rule_from_list t q) right_matrix
    | (Eqrr_var(var,recipe,cons_ax_apps) as t)::q ->
        apply_external_eqrr_phase_2
          (apply_step_b_phase_2 function_next)
          (apply_step_b_phase_2 function_next)
          (apply_rule (remove_rule_from_list t q))
          cons_ax_apps var recipe matrix
    | (Eqrr(var,recipe) as t)::q ->
        let left_matrix,right_matrix = Rules.apply_external_eqrr_phase_2 var recipe matrix in
        apply_step_b_phase_2 function_next left_matrix;
        apply_rule (remove_rule_from_list t q) right_matrix

  in

  let list_app = Constraint_system.Matrix.fold_left_row (Constraint_system.Row.fold_left search_next_rule_for_inequation) [] matrix in

  if list_app = []
  then function_next matrix
  else apply_rule list_app matrix


(********************************************
***     Step c Phase 2 of the strategy    ***
*********************************************)

let add_in_axiom_apps_phase_2 general_list new_list =

  let rec go_through (i,v,p) = function
    | [] -> [v, [i,[i,v,p]]]
    | (v',l_i)::q when Recipe.is_equal_variable v v' ->
        let rec go_through_support = function
          | [] -> [i,[i,v,p]]
          | (j,l)::q when j = i ->
              if List.exists (fun (_,_,p') -> Recipe.is_equal_path p p') l
              then (j,l)::q
              else (j,(i,v,p)::l)::q
          | t::q -> t::(go_through_support q)
        in

        (v',go_through_support l_i)::q
    | t::q -> t::(go_through (i,v,p) q)
  in

  List.fold_left (fun acc elt -> go_through elt acc) general_list new_list

let create_axiom_apps_phase_2 ax_apps =

  List.fold_left (fun acc (_,l) ->
    List.fold_left (fun acc' (_,l') ->
      l' @ acc'
    ) acc l
  ) [] ax_apps

exception No_rule_applied_step_c_p2 of Constraint_system.matrix

let apply_step_c_phase_2 function_next matrix =

  let map_csys axiom_apps csys =
    if Constraint_system.is_bottom csys
    then csys
    else
    let frame = Constraint_system.get_frame csys in
    let dc_set = Constraint_system.get_deducibility_constraint_set csys in

    Constraint_system.Phase_2.map_message_inequations (fun m_formula assoc_table ->
      match assoc_table with
      | None ->
          Term.find_and_apply_formula (fun t1 t2 ->
            (* t1 must be a variable *)
            if Term.is_variable t2
            then false
            else if Term.exists_name t2
            then
              let var_dc = Recipe.variable_of_recipe (Constraint_system.Phase_2.recipe_of_term csys t1) in
              let dc,_ = Constraint.search
                (Constraint.SUnique (Recipe.get_support var_dc))
                (fun dc -> Recipe.is_equal_variable var_dc (Constraint.Deducibility.get_recipe_variable dc))
                dc_set
              in

              let ax_apps = get_axiom_apps_phase_2 frame dc var_dc (Term.variable_of_term t1) t2 in

              if ax_apps = []
              then
                if Term.is_name t2
                then
                  true
                else Constraint.Deducibility.is_noCons dc (Term.top t2)
              else
                (axiom_apps := add_in_axiom_apps_phase_2 !axiom_apps ax_apps; false)
            else
              let mpc_t1 = Constraint_system.Phase_2.get_max_param_context_from_term csys t1
              and mpc_t2 = Constraint_system.Phase_2.get_max_param_context_from_term csys t2 in

              if mpc_t1 < mpc_t2
              then
                let var_dc = Recipe.variable_of_recipe (Constraint_system.Phase_2.recipe_of_term csys t1) in
                let dc,_ = Constraint.search
                  (Constraint.SUnique (Recipe.get_support var_dc))
                  (fun dc -> Recipe.is_equal_variable var_dc (Constraint.Deducibility.get_recipe_variable dc))
                  dc_set
                in
                let ax_apps = get_axiom_apps_phase_2 frame dc var_dc (Term.variable_of_term t1) t2 in
                if ax_apps = []
                then Constraint.Deducibility.is_noCons dc (Term.top t2)
                else (axiom_apps := add_in_axiom_apps_phase_2 !axiom_apps ax_apps; false)
              else false
          ) (fun _ _ -> Term.top_formula,None) (fun () -> m_formula,assoc_table) m_formula
      | Some(r_formula) ->
          let no_path = ref true in
          let possible_ax = ref [] in

          Recipe.find_and_apply_formula (fun c1 c2 ->
            if not (Recipe.is_variable_context c1)
            then (no_path := false; false)
            else if Recipe.is_path_context c2
            then (no_path := false; false)
            else
              let r1 = Recipe.recipe_of_context c1 in
              let var_dc = Recipe.variable_of_recipe r1 in
              let dc,_ = Constraint.search
                (Constraint.SUnique (Recipe.get_support var_dc))
                (fun dc -> Recipe.is_equal_variable var_dc (Constraint.Deducibility.get_recipe_variable dc))
                dc_set
              in
              let t1 = Constraint.Deducibility.get_message dc in
              let t2 = Term.find_and_apply_formula (fun t1' _ -> Term.is_equal_term t1 t1') (fun _ t2 -> t2)
                (fun () -> Debug.internal_error "[strategy.ml >> apply_step_c_phase_2] There should be a corresponding variable")
                m_formula
              in

              let ax_apps = get_axiom_apps_phase_2 frame dc var_dc (Term.variable_of_term t1) t2 in
              if ax_apps = []
              then Constraint.Deducibility.is_noCons dc (Term.top t2)
              else (possible_ax := ax_apps @ !possible_ax; false)
          ) (fun _ _ -> Term.top_formula,None) (fun () ->
              if not !no_path
              then axiom_apps := add_in_axiom_apps_phase_2 !axiom_apps !possible_ax;

              m_formula,assoc_table
          ) r_formula
    ) csys
  in

  let rec apply_all matrix =
    let axiom_apps = ref [] in
    let matrix' = Constraint_system.Matrix.map (map_csys axiom_apps) matrix in

    let axiom_apps' = create_axiom_apps_phase_2 !axiom_apps in
    if axiom_apps' = []
    then function_next matrix'
    else apply_external_axiom_phase_2 apply_all apply_all axiom_apps' matrix'
  in

  let axiom_apps = ref [] in
  let matrix' = Constraint_system.Matrix.map (map_csys axiom_apps) matrix in
  let axiom_apps' = create_axiom_apps_phase_2 !axiom_apps in

  if axiom_apps' = []
  then raise (No_rule_applied_step_c_p2 matrix')
  else apply_external_axiom_phase_2 apply_all apply_all axiom_apps' matrix'

(********************************************
***        Phase 2 of the strategy        ***
*********************************************)

let rec apply_cycle_b_c_phase_2 function_next matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->

    Constraint_system.Matrix.iter (fun csys ->
      if not (Constraint_system.is_bottom csys) && Constraint.exists Constraint.SAll (fun dc ->
        not (Term.is_variable (Constraint.Deducibility.get_message dc))
        || (Constraint.exists Constraint.SAll (fun dc' ->
              not (Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable dc) (Constraint.Deducibility.get_recipe_variable dc'))
              && (Term.is_equal_term (Constraint.Deducibility.get_message dc) (Constraint.Deducibility.get_message dc'))
           ) (Constraint_system.get_deducibility_constraint_set csys))
      ) (Constraint_system.get_deducibility_constraint_set csys)
      then Debug.internal_error "[Invariant 1] The invariant RightHandVar is not satisfied"
    ) matrix;
  );
  (***[END DEBUG]***)

  apply_step_b_phase_2 (fun matrix_1 ->
    (***[Statistic]***)
    Statistic.record_matrix Statistic.PTwo_SB matrix_1;

    try
      (***[BEGIN DEBUG]***)
      Debug.high_debugging (fun () ->
        Constraint_system.Matrix.iter (fun csys ->
          if not (Constraint_system.is_bottom csys) && Constraint.exists Constraint.SAll (fun dc ->
            not (Term.is_variable (Constraint.Deducibility.get_message dc))
            || (Constraint.exists Constraint.SAll (fun dc' ->
              not (Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable dc) (Constraint.Deducibility.get_recipe_variable dc'))
              && (Term.is_equal_term (Constraint.Deducibility.get_message dc) (Constraint.Deducibility.get_message dc'))
           ) (Constraint_system.get_deducibility_constraint_set csys))
            ) (Constraint_system.get_deducibility_constraint_set csys)
          then Debug.internal_error "[Invariant 2] The invariant RightHandVar is not satisfied"
        ) matrix_1;
      );
      (***[END DEBUG]***)

      apply_step_c_phase_2 (fun matrix_2 ->
        (***[Statistic]***)
        Statistic.record_matrix Statistic.PTwo_SC matrix_2;

        (***[BEGIN DEBUG]***)
        Debug.high_debugging (fun () ->
          Constraint_system.Matrix.iter (fun csys ->
            if not (Constraint_system.is_bottom csys) && Constraint.exists Constraint.SAll (fun dc ->
              not (Term.is_variable (Constraint.Deducibility.get_message dc))
              || (Constraint.exists Constraint.SAll (fun dc' ->
              not (Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable dc) (Constraint.Deducibility.get_recipe_variable dc'))
              && (Term.is_equal_term (Constraint.Deducibility.get_message dc) (Constraint.Deducibility.get_message dc'))
           ) (Constraint_system.get_deducibility_constraint_set csys))
              ) (Constraint_system.get_deducibility_constraint_set csys)
            then Debug.internal_error "[Invariant 3] The invariant RightHandVar is not satisfied"
          ) matrix_2;
        );
        (***[END DEBUG]***)


        apply_cycle_b_c_phase_2 function_next matrix_2) matrix_1
    with
      No_rule_applied_step_c_p2 matrix_2->
        function_next matrix_2
  ) matrix

let apply_phase_2 function_next matrix =
  (***[BEGIN DEBUG]***)
  Debug.high_debugging (fun () ->
    Constraint_system.Matrix.iter (fun csys ->
      if not (Constraint_system.is_bottom csys) && Constraint.exists Constraint.SAll (fun dc ->
        not (Term.is_variable (Constraint.Deducibility.get_message dc))
        || (Constraint.exists Constraint.SAll (fun dc' ->
              not (Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable dc) (Constraint.Deducibility.get_recipe_variable dc'))
              && (Term.is_equal_term (Constraint.Deducibility.get_message dc) (Constraint.Deducibility.get_message dc'))
           ) (Constraint_system.get_deducibility_constraint_set csys))
      ) (Constraint_system.get_deducibility_constraint_set csys)
      then (
        Printf.printf "%s\n" (Constraint_system.Matrix.display matrix);
        Debug.internal_error "[Invariant 0] The invariant RightHandVar is not satisfied")
    ) matrix;
  );
  (***[END DEBUG]***)

  if Constraint_system.Matrix.is_empty matrix
  then function_next matrix
  else apply_step_a_phase_2 (fun matrix_1 ->
    (***[Statistic]***)
    Statistic.record_matrix Statistic.PTwo_SA matrix_1;

    apply_cycle_b_c_phase_2 function_next matrix_1
    ) matrix

(********************************************
***        Matrix Phase 1 - Phase2        ***
*********************************************)

let phase_2_to_phase_1 matrix =
  Constraint_system.Matrix.map (Constraint_system.Phase_1.activate_phase) matrix

let phase_1_to_phase_2 matrix =
  Constraint_system.Matrix.map (Constraint_system.Phase_2.activate_phase) matrix

(********************************************
***              The strategy             ***
*********************************************)

let apply_strategy_input function_next matrix =
  apply_phase_1_input (Constraint_system.Matrix.get_maximal_support matrix) (fun matrix_1 ->
    apply_phase_2 (fun matrix_2 ->
      (***[Statistic]***)
      Statistic.record_matrix Statistic.Leaf matrix_2;

      function_next (phase_2_to_phase_1 matrix_2)
    ) (phase_1_to_phase_2 matrix_1)
  ) matrix

let apply_strategy_output function_next matrix =
  apply_phase_1_output (Constraint_system.Matrix.get_maximal_support matrix) (fun matrix_1 ->
    apply_phase_2 (fun matrix_2 ->
      (***[Statistic]***)
      Statistic.record_matrix Statistic.Leaf matrix_2;

      function_next (phase_2_to_phase_1 matrix_2)
    ) (phase_1_to_phase_2 matrix_1)
  ) matrix

let apply_full_strategy function_next matrix =
  apply_phase_1 (fun matrix_1 ->
    apply_phase_2 (fun matrix_2 ->
      (***[Statistic]***)
      Statistic.record_matrix Statistic.Leaf matrix_2;

      function_next (phase_2_to_phase_1 matrix_2)
    ) (phase_1_to_phase_2 matrix_1)
  ) matrix
