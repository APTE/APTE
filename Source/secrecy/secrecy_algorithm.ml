open Standard_library

exception Not_secrecy of Process.symbolic_process


(*********************************
  In this file contains the different strategy on the
  symbolic processes that we consider in the tool. In
  particular, it includes the strategies:
    - with(out) internal communication
    - full unfolding of interleaving
    - alternating interleaving / csys
        -- with erase double
    - partial order reductions (compression, reduction, improper killing, etc.)
**********************************)


(************************************
***    Partition of the matrix    ***
*************************************)

let partionate_matrix function_next symb_proc matrix =
  let nb_column = Constraint_system.Matrix.get_number_column matrix in

  if nb_column <> 1 then Debug.internal_error "The number of column in the matrix should always be one for secrecy problems";

  try
    let csys,_ = Constraint_system.Matrix.find_in_col 1 (fun csys -> not (Constraint_system.is_bottom csys)) matrix in
    let symb_proc' = Process.replace_constraint_system csys symb_proc in
    function_next symb_proc'
  with
  | Not_found -> ()

(*************************************
***    Functions for the strategy  ***
**************************************)

(** The initial final test *)

let final_test_on_matrix p_symb matrix =
  let nb_column = Constraint_system.Matrix.get_number_column matrix in

  if nb_column <> 1 then Debug.internal_error "The number of column in the matrix should always be one for secrecy problems";

  let nb_line = Constraint_system.Matrix.get_number_line matrix in

  for i = 1 to nb_line do
    try
      let csys,_ = Constraint_system.Matrix.find_in_row i (fun csys ->
          let frame = Constraint_system.get_frame csys in
          Constraint.exists Constraint.SAll (fun frame_elt ->
            let t = Constraint.Frame.get_message frame_elt in
            Term.is_equal_term t (Term.term_of_name Term.name_bad)
            ) frame
        ) matrix in
      raise (Not_secrecy (Process.replace_constraint_system csys p_symb))
    with
    | Not_found -> ()
  done

(** Strategy for the matrices of constraint systems *)

let apply_strategy_for_matrices next_function strategy_for_matrix proc_symb =

  let matrix = Constraint_system.Matrix.matrix_of_row_matrix (Constraint_system.Row.create 1 [Process.get_constraint_system proc_symb]) in

  (* ** Application of the strategy on matrices *)

  strategy_for_matrix (fun matrix_1 ->
    if Constraint_system.Matrix.is_empty matrix_1
    then ()
    else
      begin
        final_test_on_matrix proc_symb matrix_1;
        next_function matrix_1
      end
  ) matrix

  (** Strategy for the complete unfolding without POR *)

  let apply_strategy_one_transition pub_channels next_function_output next_function_input symb_proc =

    (* ** First step : apply the internal transitions (including conditionals) *)

    let internal = ref [] in

    (* Scan all symbolic processes, flatten all parallels/choices and perform all available
     conditionals and branch for then/else and for the different ways (disjunction)
     to satisfy the conditional's test. Thanks to our "function_next", we then put
     all those alternatives together in left/right_internal lists. *)
    List.iter (fun symb_proc_1 ->
      Process.apply_internal_transition
        !Algorithm.option_semantics
        pub_channels
        ~with_por:false ~with_improper:false (fun symb_proc_2 ->
        internal := symb_proc_2::!internal
      ) symb_proc_1
    ) [symb_proc];

    (* ** Second step : apply the output transitions *)
    let support = Constraint_system.get_maximal_support (Process.get_constraint_system (List.hd !internal)) in

    let output_set = ref [] in

    let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support in

    (* Scan all symbolic processes and look for one starting with an output and
     apply function_next to the resulting symbolic process. We thus store in
     left/right_output_set all the alternatives of performing an output. *)
    List.iter (fun symb_proc_1 ->
  	     Process.apply_output
  	       false (fun (symb_proc_2,_) ->
  			    let simplified_symb_proc = Process.simplify symb_proc_2 in
  			    if not (Process.is_bottom simplified_symb_proc)
  			    then output_set := simplified_symb_proc::!output_set
  			   ) var_r_ch symb_proc_1
  	    ) !internal;

    (* We pass those alternatives to the next step which consists in:
       1. put all csys in a row matrix
       2. apply Strategy.apply_strategy_input/output resulting in a branching
          process ending with many matrices (for leaves: in solved form)
       3. apply final_test_on_matrix on all those leaves, if OK:
       4. apply partitionate_matrix giving many pairs of symbolic processes
       5. recursive calls on each of them to keep exploring actions
     *)
    if !output_set <> []
    then List.iter next_function_output !output_set;

    (* ** Third step : apply the input transitions *)

    let input_set = ref [] in

    let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support
    and var_r_t = Recipe.fresh_free_variable_from_id "Y" support in

    (* Scan all symbolic processes and look for one starting with an input and
     apply function_next to the resulting symbolic process. We thus store in
     left/right_input_set all the alternatives of performing an input. *)
    List.iter (fun symb_proc_1 ->
  	     Process.apply_input
  	       false (fun (symb_proc_2,_) ->
  			    let simplified_symb_proc = Process.simplify symb_proc_2 in
  			    if not (Process.is_bottom simplified_symb_proc)
  			    then input_set := simplified_symb_proc::!input_set
  			   ) var_r_ch var_r_t symb_proc_1
  	    ) !internal;

    (* We pass those alternatives to the next step (same desc. as for out.) *)
    if !input_set <> []
    then List.iter next_function_input !input_set

let rec apply_alternating pub_channels symb_proc =
    let next_function f_strat_m symb_proc_1 =
      (***[Statistic]***)
      Trace_equivalence.Statistic.start_transition [symb_proc_1] [];

      apply_strategy_for_matrices (fun matrix ->
          partionate_matrix (apply_alternating pub_channels) symb_proc_1 matrix
        ) f_strat_m symb_proc_1;

      (***[Statistic]***)
      Trace_equivalence.Statistic.end_transition ()
    in

    apply_strategy_one_transition
      pub_channels
      (next_function Trace_equivalence.Strategy.apply_strategy_output)
      (next_function Trace_equivalence.Strategy.apply_strategy_input)
      symb_proc


let decide_secrecy process =
  (* We assume at this point that all name in the process are distinct *)

  (* Get the free names *)
  let free_names = Process.get_free_names process in

  let pub_channels =
    match Process.is_well_typed process with
      | None -> Algorithm.option_semantics := Process.Classic; []
      | Some l -> Algorithm.option_semantics := Process.Private; l
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

  (* Creation of the symbolic process *)
  let symb_proc = Process.create_symbolic assoc_axiom_free_names process csys in

  (* Application of the strategy *)
  try
    apply_alternating pub_channels symb_proc;
    true
  with
    | Not_secrecy sym_proc ->
        Printf.printf "Witness of non-preservation of secrecy on the process :\n%s"
          (Process.display_trace (Process.instanciate_trace sym_proc));
        false
