open Standard_library

exception Not_equivalent_left of Process.symbolic_process
exception Not_equivalent_right of Process.symbolic_process


(*********************************
  In this file contains the different strategy on the
  symbolic processes that we consider in the tool. In
  particular, it includes the strategies:
    - with(out) internal communication
    - full unfolding of interleaving
    - alternating interleaving / csys
        -- with erase double 
**********************************)

(** Parameters *)

(* TODO: for the release, set booleans to false, true, true, true *)
let option_por = ref true

let option_internal_communication = ref false

let option_erase_double = ref false

let option_alternating_strategy = ref true
  
(************************************
***    Partition of the matrix    ***
*************************************)
  
let rec add_left_in_partition bool_rep symb_proc = function
  | [] -> [bool_rep, [symb_proc], []]
  | (b_rep,left_symb,right_symb)::q when List.for_all2 (fun b1 b2 -> b1 = b2) b_rep bool_rep ->
      (b_rep,symb_proc::left_symb,right_symb)::q
  | rep::q -> rep::(add_left_in_partition bool_rep symb_proc q)

let rec add_right_in_partition bool_rep symb_proc = function
  | [] -> Debug.internal_error "[algorithm.ml >> add_right_in_partition] A boolean representation must be found in the partition"
  | (b_rep,left_symb,right_symb)::q when List.for_all2 (fun b1 b2 -> b1 = b2) b_rep bool_rep ->
      (b_rep,left_symb,symb_proc::right_symb)::q
  | rep::q -> rep::(add_right_in_partition bool_rep symb_proc q)

let partionate_matrix function_next left_symb_proc right_symb_proc index_right_process  matrix = 
  let nb_column = Constraint_system.Matrix.get_number_column matrix in
  
  let partition = ref [] in
  
  for j = 1 to index_right_process - 1 do
    let (bool_rep, csys) = 
      Constraint_system.Matrix.fold_left_on_column j (fun (bool_l,last_csys) csys ->
        if Constraint_system.is_bottom csys
        then (0::bool_l,last_csys)
        else (1::bool_l,csys)
      ) ([],Constraint_system.bottom) matrix
    in
    
    if Constraint_system.is_bottom csys
    then ()
    else 
      let old_symb_proc = List.nth left_symb_proc (j-1) in
      partition := add_left_in_partition bool_rep (Process.replace_constraint_system csys old_symb_proc) !partition
  done;
  
  for j = index_right_process to nb_column do
    let (bool_rep, csys) = 
      Constraint_system.Matrix.fold_left_on_column j (fun (bool_l,last_csys) csys ->
        if Constraint_system.is_bottom csys
        then (0::bool_l,last_csys)
        else (1::bool_l,csys)
      ) ([],Constraint_system.bottom) matrix
    in
    
    if Constraint_system.is_bottom csys
    then ()
    else 
      let old_symb_proc = List.nth right_symb_proc (j - index_right_process) in
      partition := add_right_in_partition bool_rep (Process.replace_constraint_system csys old_symb_proc) !partition
  done;
  
  List.iter (fun (_,left_symb,right_symb) -> function_next left_symb right_symb) !partition

(************************************
***          Erase double         ***
*************************************)

(** [erase_double] is only effective if the trace of the symbolic processes have been instantiated. *)
let rec erase_double = function 
  | [] -> []
  | symb_csys::q when List.exists (Process.is_same_input_output symb_csys) q -> erase_double q
  | symb_csys::q -> symb_csys::(erase_double q)

(*************************************
***    Functions for the strategy  ***
**************************************)

(** The initial final test *)

let final_test_on_matrix index_right_process left_set right_set matrix = 
  let nb_column = Constraint_system.Matrix.get_number_column matrix in
  
  let nb_line = Constraint_system.Matrix.get_number_line matrix in
 
  for i = 1 to nb_line do
    if index_right_process = nb_column + 1
    then
      try
        let left_csys,j = Constraint_system.Matrix.find_in_row i (fun csys -> not (Constraint_system.is_bottom csys)) matrix in
        let symb_proc = List.nth left_set (j-1) in
        let symb_proc' = Process.replace_constraint_system left_csys symb_proc in
        raise (Not_equivalent_left symb_proc')
      with Not_found -> ()
    else if index_right_process = 1
    then
      try
        let right_csys,j = Constraint_system.Matrix.find_in_row i (fun csys -> not (Constraint_system.is_bottom csys)) matrix in
        let symb_proc = List.nth right_set (j-1) in
        let symb_proc' = Process.replace_constraint_system right_csys symb_proc in
        raise (Not_equivalent_right symb_proc')
      with Not_found -> ()
    else
      try
        let left_csys,j = Constraint_system.Matrix.find_in_row_between_col_index i 1 (index_right_process - 1) (fun csys -> not (Constraint_system.is_bottom csys)) matrix in
      
        if Constraint_system.Matrix.exists_in_row_between_col_index i index_right_process nb_column (fun csys -> not (Constraint_system.is_bottom csys)) matrix
        then ()
        else 
          let symb_proc = List.nth left_set (j-1) in
          let symb_proc' = Process.replace_constraint_system left_csys symb_proc in
          raise (Not_equivalent_left (symb_proc'))
      with Not_found ->
        begin
          try
            let right_csys,j = Constraint_system.Matrix.find_in_row_between_col_index i index_right_process nb_column (fun csys -> not (Constraint_system.is_bottom csys)) matrix in
            let symb_proc = List.nth right_set (j-index_right_process) in
            let symb_proc' = Process.replace_constraint_system right_csys symb_proc in
            raise (Not_equivalent_right (symb_proc'))
          with Not_found -> ()
        end  
  done

(** Strategy for the matrices of constraint systems *)

let apply_strategy_for_matrices next_function strategy_for_matrix left_set right_set =
  
  let number_left_symb_proc = List.length left_set
  and number_right_symb_proc = List.length right_set in
  
  let index_right_process = number_left_symb_proc + 1 in

  (* ** Creation of the matrix *)
  
  let complete_csys_list = 
    List.fold_right (fun left_symb acc -> 
      (Process.get_constraint_system left_symb)::acc
    ) left_set (List.map Process.get_constraint_system right_set)
  in
  
  let matrix = Constraint_system.Matrix.matrix_of_row_matrix 
    (Constraint_system.Row.create
      (number_left_symb_proc + number_right_symb_proc)
      complete_csys_list
    )
  in
  
  (* ** Application of the strategy on matrices *)
  
  strategy_for_matrix (fun matrix_1 ->
    if Constraint_system.Matrix.is_empty matrix_1
    then ()
    else
      begin
        final_test_on_matrix index_right_process left_set right_set matrix_1;
        next_function index_right_process matrix_1
      end
  ) matrix


(** Strategy for the complete unfolding without POR *)

let apply_strategy_one_transition next_function_output next_function_input left_symb_proc_list right_symb_proc_list = 

  (* ** Option Erase Double *)
  
  let left_erase_set = 
    if !option_erase_double 
    then erase_double (List.map Process.instanciate_trace left_symb_proc_list)
    else left_symb_proc_list
  and right_erase_set =
    if !option_erase_double 
    then erase_double (List.map Process.instanciate_trace right_symb_proc_list)
    else right_symb_proc_list
  in


  (* ** First step : apply the internal transitions (including conditionals) *)
  
  let left_internal = ref []
  and right_internal = ref [] in
  
  (* Scan all symbolic processes, flatten all parallels/choices and perform all available
   conditionals and branch  for then/else and for the different ways (disjunction)
   to satisfy the  conditional's test. Thanks to our "function_next", we then pu
   all those alternatives together in left/right_internal lists. *)
  List.iter (fun symb_proc_1 ->
    Process.apply_internal_transition !option_internal_communication !option_por (fun symb_proc_2 -> 
      left_internal := symb_proc_2::!left_internal
    ) symb_proc_1
  ) left_erase_set;
  
  List.iter (fun symb_proc_1 ->
    Process.apply_internal_transition !option_internal_communication !option_por (fun symb_proc_2 -> 
      right_internal := symb_proc_2::!right_internal
    ) symb_proc_1
  ) right_erase_set;
  

  (* ** Second step : apply the output transitions *)
  
  let support = 
    if !left_internal = []
    then Constraint_system.get_maximal_support (Process.get_constraint_system (List.hd !right_internal))
    else Constraint_system.get_maximal_support (Process.get_constraint_system (List.hd !left_internal))
  in
  
  let left_output_set = ref []
  and right_output_set = ref [] in
  
  let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support in
  
  (* Scan all symbolic processes and look for one starting with an output and
   apply function_next to the resulting symbolic process. We thus store in
   left/right_output_set all the alternatives of performing an output. *)
  List.iter (fun symb_proc_1 ->
	     Process.apply_output
	       !option_por (fun (symb_proc_2,_) -> 
			    let simplified_symb_proc = Process.simplify symb_proc_2 in
			    if not (Process.is_bottom simplified_symb_proc)
			    then left_output_set := simplified_symb_proc::!left_output_set
			   ) var_r_ch symb_proc_1
	    ) !left_internal;
  
  List.iter (fun symb_proc_1 ->
	     Process.apply_output
	       !option_por (fun (symb_proc_2,_) -> 
			    let simplified_symb_proc = Process.simplify symb_proc_2 in
			    if not (Process.is_bottom simplified_symb_proc)
			    then right_output_set := simplified_symb_proc::!right_output_set
			   ) var_r_ch symb_proc_1
	    ) !right_internal;
  
  (* We pass those alternatives to the next step which consists in:
     1. put all csys in a row matrix
     2. apply Strategy.apply_strategy_input/output resulting in a branching
        process ending with many matrices (for leaves: in solved form)
     3. apply final_test_on_matrix on all those leaves, if OK:
     4. apply partitionate_matrix giving many pairs of symbolic processes
     5. recursive calls on each of them
   *)
  if !left_output_set <> [] || !right_output_set <> []
  then next_function_output !left_output_set !right_output_set;


  (* ** Third step : apply the input transitions *)
  
  let left_input_set = ref []
  and right_input_set = ref [] in
  
  let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support
  and var_r_t = Recipe.fresh_free_variable_from_id "Y" support in
  
  (* Scan all symbolic processes and look for one starting with an input and
   apply function_next to the resulting symbolic process. We thus store in
   left/right_input_set all the alternatives of performing an input. *)
  List.iter (fun symb_proc_1 ->
	     Process.apply_input
	       !option_por (fun (symb_proc_2,_) -> 
			    let simplified_symb_proc = Process.simplify symb_proc_2 in
			    if not (Process.is_bottom simplified_symb_proc)
			    then left_input_set := simplified_symb_proc::!left_input_set
			   ) var_r_ch var_r_t symb_proc_1
	    ) !left_internal;
  
  List.iter (fun symb_proc_1 ->
	     Process.apply_input
	       !option_por (fun (symb_proc_2,_) -> 
			    let simplified_symb_proc = Process.simplify symb_proc_2 in
			    if not (Process.is_bottom simplified_symb_proc)
			    then right_input_set := simplified_symb_proc::!right_input_set
			   ) var_r_ch var_r_t symb_proc_1
	    ) !right_internal;
  
  (* We pass those alternatives to the next step (same desc. as for out.) *)
  if !left_input_set <> [] || !right_input_set <> []
  then next_function_input !left_input_set !right_input_set


(** Strategy for the complete unfolding with POR *)
let apply_strategy_one_transition_por next_function_output next_function_input left_symb_proc_list right_symb_proc_list = 

  Printf.printf "Avant de commencer apply_strategy_one. Taille des listes: %d,%d.\n"
		(List.length left_symb_proc_list)
		(List.length right_symb_proc_list);

  (* ** First step : apply the internal transitions (including conditionals) *)
  
  let left_internal = ref []
  and right_internal = ref [] in
  
  (* Scan all symbolic processes, flatten all parallels/choices and perform all available
   conditionals and branch  for then/else and for the different ways (disjunction)
   to satisfy the  conditional's test. Thanks to our "function_next", we then pu
   all those alternatives together in left/right_internal lists. *)
  List.iter (fun symb_proc_1 ->
    Process.apply_internal_transition !option_internal_communication true (fun symb_proc_2 -> 
      left_internal := symb_proc_2::!left_internal
    ) symb_proc_1
  ) left_symb_proc_list;
  
  List.iter (fun symb_proc_1 ->
    Process.apply_internal_transition !option_internal_communication true (fun symb_proc_2 -> 
      right_internal := symb_proc_2::!right_internal
    ) symb_proc_1
  ) right_symb_proc_list;
  

  (* ** Second step : apply the output transitions *)
  Printf.printf "Après les TESTS. Taille des listes: %d,%d.\n"
		(List.length !left_internal)
		(List.length !right_internal);
  
  let support = 
    if !left_internal = []
    then Constraint_system.get_maximal_support (Process.get_constraint_system (List.hd !right_internal))
    else Constraint_system.get_maximal_support (Process.get_constraint_system (List.hd !left_internal))
  in
  
  let left_output_set = ref []
  and right_output_set = ref [] in
  
  let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support in
  
  (* Scan all symbolic processes and look for one starting with an output and
   apply function_next to the resulting symbolic process. We thus store in
   left/right_output_set all the alternatives of performing an output. *)
  
  (* Idée: on va chercher le canal du premier output qu'on trouve parmi
   les processus DES left_internal. Si il y en a pas alors on est positif
   à gauche: dans ce cas on check qu'on l'est à droite et on passe en mode input.
   Sinon, on récupére le canal de cet output et on applique les List.iter
   pour CET output/canal. A CHECK: label, channel en mode Term.term suffisant?
   ETC... *)
  
  List.iter (fun symb_proc_1 ->
	     Process.apply_output
	       !option_por (fun (symb_proc_2,_) -> 
			    let simplified_symb_proc = Process.simplify symb_proc_2 in
			    if not (Process.is_bottom simplified_symb_proc)
			    then left_output_set := simplified_symb_proc::!left_output_set
			   ) var_r_ch symb_proc_1
	    ) !left_internal;
  
  List.iter (fun symb_proc_1 ->
	     Process.apply_output
	       !option_por (fun (symb_proc_2,_) -> 
			    let simplified_symb_proc = Process.simplify symb_proc_2 in
			    if not (Process.is_bottom simplified_symb_proc)
			    then right_output_set := simplified_symb_proc::!right_output_set
			   ) var_r_ch symb_proc_1
	    ) !right_internal;
  
  (* We pass those alternatives to the next step which consists in:
     1. put all csys in a row matrix
     2. apply Strategy.apply_strategy_input/output resulting in a branching
        process ending with many matrices (for leaves: in solved form)
     3. apply final_test_on_matrix on all those leaves, if OK:
     4. apply partitionate_matrix giving many pairs of symbolic processes
     5. recursive calls on each of them
   *)
  Printf.printf "Après les OUT. Taille des listes: %d,%d.\n"
		(List.length !left_output_set)
		(List.length !right_output_set);

  if !left_output_set <> [] || !right_output_set <> []
  then next_function_output !left_output_set !right_output_set;





(*   (\* two following lists: associations lists: one channel -> list of corresponding alternatives *)
(* (from executing output on this channel) *\) *)
(*   let left_output_set_channel : (((Term.term * Process.symbolic_process list ref) list) ref) = ref [] *)
(*   and right_output_set_channel : (((Term.term * Process.symbolic_process list ref) list) ref) = ref [] in *)
(*     let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support in *)
(*   List.iter (fun symb_proc_1 -> *)
(* 	     Process.apply_output *)
(* 	       true (fun (symb_proc_2,ch) ->  *)
(* 		     (\* For any resulting symbolic process from executing an output on channel ch: *\) *)
(* 		     let simplified_symb_proc = Process.simplify symb_proc_2 in *)
(* 		     if not (Process.is_bottom simplified_symb_proc) *)
(* 		     then (try begin *)
(* 			       let l_ch = List.assoc ch !left_output_set_channel in *)
(* 			       l_ch := simplified_symb_proc :: !l_ch; *)
(* 			     end with *)
(* 			   | Not_found ->  *)
(* 			      left_output_set_channel := (ch, ref [simplified_symb_proc])::!left_output_set_channel) *)
(* 		    ) var_r_ch symb_proc_1 *)
(* 	    ) !left_internal; *)
(*   List.iter (fun symb_proc_1 -> *)
(* 	     Process.apply_output *)
(* 	       true (fun (symb_proc_2,ch) ->  *)
(* 		     (\* For any resulting symbolic process from executing an output on channel ch: *\) *)
(* 		     let simplified_symb_proc = Process.simplify symb_proc_2 in *)
(* 		     if not (Process.is_bottom simplified_symb_proc) *)
(* 		     then (try begin *)
(* 			       let l_ch = List.assoc ch !left_output_set_channel in *)
(* 			       l_ch := simplified_symb_proc :: !l_ch; *)
(* 			     end with *)
(* 			   | Not_found ->  *)
(* 			      left_output_set_channel := (ch, ref [simplified_symb_proc])::!left_output_set_channel) *)
(* 		    ) var_r_ch symb_proc_1 *)
(* 	    ) !right_internal; *)

		 
  (* ** Third step : apply the input transitions *)
  

  let left_input_set = ref []
  and right_input_set = ref [] in
  
  let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support
  and var_r_t = Recipe.fresh_free_variable_from_id "Y" support in
  
  (* Scan all symbolic processes and look for one starting with an input and
   apply function_next to the resulting symbolic process. We thus store in
   left/right_input_set all the alternatives of performing an input. *)
  List.iter (fun symb_proc_1 ->
    Process.apply_input !option_por (fun (symb_proc_2,ch) -> 
      let simplified_symb_proc = Process.simplify symb_proc_2 in
      if not (Process.is_bottom simplified_symb_proc)
      then left_input_set := simplified_symb_proc::!left_input_set
    ) var_r_ch var_r_t symb_proc_1
  ) !left_internal;
  
  List.iter (fun symb_proc_1 ->
    Process.apply_input !option_por (fun (symb_proc_2,ch) -> 
      let simplified_symb_proc = Process.simplify symb_proc_2 in
      if not (Process.is_bottom simplified_symb_proc)
      then right_input_set := simplified_symb_proc::!right_input_set
    ) var_r_ch var_r_t symb_proc_1
  ) !right_internal;
    
  Printf.printf "Après les IN. Taille des listes: %d,%d.\n"
		(List.length !left_input_set)
		(List.length !right_input_set);

  (* We pass those alternatives to the next step (same desc. as for out.) *)
  if !left_input_set <> [] || !right_input_set <> []
  then next_function_input !left_input_set !right_input_set


(*************************************
***         The strategies         ***
**************************************)  
  
(** The complete unfolding strategy *)

let rec apply_complete_unfolding left_symb_proc_list right_symb_proc_list = 
  let next_function left_list right_list = 
    
    (***[Statistic]***)
    Statistic.start_transition left_list right_list;
    
    apply_strategy_for_matrices (fun _ _ -> ()) Strategy.apply_full_strategy left_list right_list;
    
    (***[Statistic]***)
    Statistic.end_transition ();
    
    apply_complete_unfolding left_list right_list
    
  in

  apply_strategy_one_transition next_function next_function left_symb_proc_list right_symb_proc_list
  
(** The alternating strategy *)

let rec apply_alternating left_symb_proc_list right_symb_proc_list =
  (* 'next_function [some Strategy]' will be applied on every pairs resulting from
   the execution of one symbolic action *)
  let next_function f_strat_m left_list right_list =
    (***[Statistic]***)
    Statistic.start_transition left_list right_list;
    
    apply_strategy_for_matrices (fun index_right_process matrix -> 
        partionate_matrix apply_alternating left_list right_list index_right_process matrix
      ) f_strat_m left_list right_list;
      
    (***[Statistic]***)
    Statistic.end_transition ()
  
  and strategy_one_transition = if !option_por
				then apply_strategy_one_transition_por
				else apply_strategy_one_transition
  in
  strategy_one_transition 
    (next_function Strategy.apply_strategy_output) 
    (next_function Strategy.apply_strategy_input)
    left_symb_proc_list
    right_symb_proc_list

    
(*****************************************
***      Decide trace equivalence      ***
******************************************)  
  
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
    if !option_alternating_strategy
    then apply_alternating [symb_proc1] [symb_proc2]
    else apply_complete_unfolding [symb_proc1] [symb_proc2];
    true
  with
    | Not_equivalent_left sym_proc ->
        Printf.printf "Witness of non-equivalence on Process 1:\n%s"
          (Process.display_trace (Process.instanciate_trace sym_proc));
        false
    | Not_equivalent_right sym_proc ->
        Printf.printf "Witness of non-equivalence on Process 2:\n%s"
          (Process.display_trace (Process.instanciate_trace sym_proc));
        false
  
  
  
