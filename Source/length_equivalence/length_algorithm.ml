open Standard_library

let test_for_length_on_leaves left_set right_set =

  let left_poly_set =
    List.map (fun proc_symb ->
      Length.polynomial_of_constraint_system (Process.get_constraint_system proc_symb)
    ) left_set
  and right_poly_set =
    List.map (fun proc_symb ->
      Length.polynomial_of_constraint_system (Process.get_constraint_system proc_symb)
    ) right_set
  in

  let equal_seq_poly poly_1 poly_2 =
    List.for_all2 Length.is_equal_polynomial poly_1 poly_2
  in

  List.iter2 (fun poly_left csys_left ->
    if List.exists (fun poly_right -> equal_seq_poly poly_left poly_right) right_poly_set
    then ()
    else raise (Trace_equivalence.Algorithm.Not_equivalent_left csys_left)
  ) left_poly_set left_set;

  List.iter2 (fun poly_right csys_right ->
    if List.exists (fun poly_left -> equal_seq_poly poly_left poly_right) left_poly_set
    then ()
    else raise (Trace_equivalence.Algorithm.Not_equivalent_right csys_right)
  ) right_poly_set right_set

(*************************************
***         The strategies         ***
**************************************)

(** The complete unfolding strategy *)

let rec apply_complete_unfolding ?(trs=Por.emptySetTraces) pub_channels left_symb_proc_list right_symb_proc_list =
  let next_function ?(trs=Por.emptySetTraces) left_list right_list =
    (***[Statistic]***)
    Trace_equivalence.Statistic.start_transition left_list right_list;

    Trace_equivalence.Algorithm.apply_strategy_for_matrices (fun index_right_process matrix ->
        Trace_equivalence.Algorithm.partionate_matrix test_for_length_on_leaves left_list right_list index_right_process matrix
      )  Trace_equivalence.Strategy.apply_full_strategy left_list right_list;

    (***[Statistic]***)
    Trace_equivalence.Statistic.end_transition ();

    apply_complete_unfolding pub_channels left_list right_list;
  in

  Trace_equivalence.Algorithm.apply_strategy_one_transition pub_channels next_function next_function next_function left_symb_proc_list right_symb_proc_list

(** The alternating strategy *)

let rec apply_alternating ?(trs=Por.emptySetTraces) pub_channels left_symb_proc_list right_symb_proc_list =
  let next_function f_strat_m ?(trs=Por.emptySetTraces) left_list right_list =
    (***[Statistic]***)
    Trace_equivalence.Statistic.start_transition left_list right_list;

    Trace_equivalence.Algorithm.apply_strategy_for_matrices (fun index_right_process matrix ->
        Trace_equivalence.Algorithm.partionate_matrix (fun left_list' right_list' ->
            test_for_length_on_leaves left_list' right_list';
            apply_alternating pub_channels left_list' right_list'
          ) left_list right_list index_right_process matrix
      ) f_strat_m left_list right_list;

    (***[Statistic]***)
    Trace_equivalence.Statistic.end_transition ()
  in

  Trace_equivalence.Algorithm.apply_strategy_one_transition
    pub_channels
    (next_function Trace_equivalence.Strategy.apply_strategy_output)
    (next_function Trace_equivalence.Strategy.apply_strategy_input)
    (next_function Trace_equivalence.Strategy.apply_full_strategy)
    left_symb_proc_list
    right_symb_proc_list




let decide_trace_equivalence process1 process2 =
  (* We assume at this point that all name in the process are distinct *)

  (* Get the free names *)
  let free_names_1 = Process.get_free_names process1 in
  let free_names_2 = Process.get_free_names process2 in

  (* Exclusive concatenation *)
  let free_names =
    List.fold_left (fun l n ->
      if List.exists (Term.is_equal_name n) free_names_2
      then l
      else n::l
    ) free_names_2 free_names_1
  in

  let pub_channels = match !Algorithm.option_semantics with
    | Process.Classic -> []
    | Process.Private | Process.Eavesdrop ->
        begin match Process.is_well_typed process1, Process.is_well_typed process2 with
          | None,_ | _, None -> failwith "The processes should be well-typed when using the Private or Eavesdrop semantics"
          | Some l1, Some l2 ->
              List.fold_left (fun l n ->
                if List.exists (Term.is_equal_name n) l2
                then l
                else n::l
              ) l2 l1
        end
  in

  (* Creation of the constraint system *)
  let csys =
    List.fold_left (fun csys n ->
      Constraint_system.add_new_axiom csys (Term.term_of_name n)
    ) Constraint_system.empty free_names
  in

  let assoc_axiom_free_names,_ =
    List.fold_left (fun (l,i) n ->
      (Recipe.recipe_of_axiom (Recipe.axiom i), Term.term_of_name n)::l,i+1
    ) ([],1) free_names
  in

  (* Creation of the two symbolic process *)
  let symb_proc1 = Process.create_symbolic assoc_axiom_free_names process1 csys
  and symb_proc2 = Process.create_symbolic assoc_axiom_free_names process2 csys in

  (* Application of the strategy *)
  try
    if !Trace_equivalence.Algorithm.option_alternating_strategy
    then apply_alternating pub_channels [symb_proc1] [symb_proc2]
    else apply_complete_unfolding pub_channels [symb_proc1] [symb_proc2];
    true
  with
    | Trace_equivalence.Algorithm.Not_equivalent_left sym_proc ->
        Printf.printf "Witness of non-equivalence on Process 1:\n%s"
          (Process.display_trace (Process.instanciate_trace sym_proc));
        false
    | Trace_equivalence.Algorithm.Not_equivalent_right sym_proc ->
        Printf.printf "Witness of non-equivalence on Process 2:\n%s"
          (Process.display_trace (Process.instanciate_trace sym_proc));
        false
