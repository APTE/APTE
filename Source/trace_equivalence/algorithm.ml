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
(* TODO: for POR, set booleans to true, false, false, false *)
let option_por = ref false

let option_internal_communication = ref true

let option_erase_double = ref true

let option_alternating_strategy = ref true
  
let print_debug_por = ref false

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


(*  BBBBBB************************************************** *)
(*  ************************************************** *)
(** Handles the exceptions that Process.* may raise and raises
 the corresponding Algorithm exception *)
let try_P symproc_left symproc_right expr = 
  try expr with
  | Process.Not_eq_left s ->
     Printf.printf "Witness' type: %s\n" s;
     raise (Not_equivalent_left symproc_left)
  | Process.Not_eq_right s ->
     Printf.printf "Witness' type: %s\n" s;
     raise (Not_equivalent_right symproc_right);;

(** Strategy for the complete unfolding with POR *)
let apply_strategy_one_transition_por (* given .... *)
      (* what to do on continuations after performing an output *)
      (next_function_output :   (Process.symbolic_process list -> Process.symbolic_process list -> unit))
      (* what to do on continuations after performing an input *)
      (next_function_input :   (Process.symbolic_process list -> Process.symbolic_process list -> unit))
      (* all alternatives on the left *)
      (left_symb_proc_list : Process.symbolic_process list)
      (* all alternatives on the right *)
      (right_symb_proc_list : Process.symbolic_process list)
    (* ... explore all possible symbolic, observable actions in the compressed semantics,
      perform all available conditionals and parallel compositions and apply 
     next_function... on resulting processes *)
    : unit = 

  (* ** We assume that the sets are singletons and either (i) trace is empty (first call) 
   but no process can have a conditional at top level or below a Par or 
   (ii) all processes start either with an input or an output (they are in normal
   form for the internal reduction \leadsto in the paper) and the two lists contain
   only one symbolic process. *)

  if !print_debug_por then
    Printf.printf "Before starting apply_strategy_one. Size of lists: %d,%d. Trace's size: %d\n"
		  (List.length left_symb_proc_list)
		  (List.length right_symb_proc_list)
		  (Process.size_trace (List.hd left_symb_proc_list));
  
  (* We check that sets of processes are singletons *)
  if List.length left_symb_proc_list != 1 || List.length right_symb_proc_list != 1
  then Debug.internal_error "[algorithm.ml >> apply_strategy_one_transition_por] The sets of pocesses are not singletons. It may be the case that inputted processes are not action-deterministic.";
  
  (* If this is the first call of apply_strategy, then we must split parallel compositions that may be at
     top level. We assume here that there is no conditional above firsts observable actions. *)
  let proc_left, proc_right =  
    if (Process.size_trace (List.hd left_symb_proc_list)) != 0
    then (List.hd left_symb_proc_list, List.hd right_symb_proc_list) (* Case (ii) *)
    else  begin			                                     (* Case (i) *)
	(* If trace is empty (first call) then we apply internal_transition before really starting *)
	let ps = ref [] in
	Process.apply_internal_transition
	  false
	  true
	  (fun symb_proc_2 -> ps := symb_proc_2 :: !ps)
	  (List.hd left_symb_proc_list);
	let qs = ref [] in
	Process.apply_internal_transition
	  false
	  true
	  (fun symb_proc_2 -> qs := symb_proc_2 :: !qs)
	  (List.hd right_symb_proc_list);
	if (List.length !ps != 1)|| (List.length !qs != 1)
	then Debug.internal_error "[algorithm.ml >> apply_strategy_one_transition_por] The sets of pocesses after reducing conditionals are not singletons. It may be the case that inputted processes start with conditionals at top level (which is forbidden)."
	else (List.hd !ps, List.hd !qs)
      end;
  in

  (* ** FIRST step: labelises processes and update 'has_focus': at this point, some new processes coming from 
        breaking a parallel composition are in the multiset. All those new processes come from a unique parallel
        composition, so we label them in an arbitrary order but consistently with right_symb_proc_list. *)

    let proc_left_label, how_to_label = try_P proc_left proc_right (Process.labelise proc_left) in
    let proc_right_label = try_P proc_left proc_right (Process.labelise_consistently how_to_label proc_right) in
    (* Updating 'has_focus' on the left *)
    let proc_left_label =
      if Process.has_focus proc_left_label 
      then (match Process.sk_of_symp proc_left_label with
	    | Process.OutS t -> Process.set_focus false proc_left_label
	    | _ -> proc_left_label)
      else proc_left_label
    (* Updating 'has_focus' on the right *)
    and proc_rght_label =
      if Process.has_focus proc_right_label 
      then (match Process.sk_of_symp proc_right_label with
	    | Process.OutS t -> Process.set_focus false proc_right_label
	    | _ -> proc_right_label)
      else proc_right_label in
    
    (* for later, we extract the support *)
    let support = Constraint_system.get_maximal_support (Process.get_constraint_system proc_left_label) in

							
    (* ** SECOND step: Distinguish two cases whether pro_left/right_label have focus or not.
         (if they do not have the same status we raise an error. *)
    match (Process.has_focus proc_left_label, Process.has_focus proc_right_label) with
    | true, false -> begin
		     Printf.printf "Witness' type: Release focus on the left, not on the right.";
		     raise (Not_equivalent_right proc_right_label);
		   end

    | false, true -> begin
		     Printf.printf "Witness' type: Release focus on the right, not on the left.";
		     raise (Not_equivalent_left proc_left_label);
		   end
    | true, true ->
       (****************** WITH FOCUS ****************************  *)
       begin
	 (* In that case, the first process of proc_left/right_label is under focus. We perform
          their first input in case thay have the same skeleton and raise an exception otherwise.
	  We assume here that focus have been removed as soon as a negative pop out.*)
	 let sk_left, sk_right = (Process.sk_of_symp proc_left_label, Process.sk_of_symp proc_right_label) in
	 if sk_left != sk_right
	 then begin
	     Printf.printf "Witness' type: process under focus on the right does not match the one on the left.";
	     raise (Not_equivalent_right proc_right_label);
	   end else
	   begin
	     (* ** Third Step/IN step: Otherwise, there is no negative process (\ie starting with an output),
    we thus choose one process (we branch here) and perform its first input then
   do the same on the right. Resulting process ahs a focus. **)
	     
	     let left_input_set = ref []
	     and right_input_set = ref []
	     and var_r_ch = Recipe.fresh_free_variable_from_id "Z" support
	     and var_r_t = Recipe.fresh_free_variable_from_id "Y" support in
	     
	     Process.apply_input
	       true		(* true: only the first process (that is under focus) is considered *)
	       (fun (symb_proc_2,ch) -> 
		(* We do not simplify symbolic processes because it will be done
	  in the next step when performing conditionals/splittings. *)
		left_input_set := symb_proc_2::!left_input_set)
	       var_r_ch
	       var_r_t
	       proc_left_label;
	     
	     Process.apply_input
	       true             (* true: only the first process (that is under focus) is considered *)
	       (fun (symb_proc_2,ch) -> 
		(* same as above *)
		right_input_set := symb_proc_2::!right_input_set)
	       var_r_ch
	       var_r_t
	       proc_right_label;
	     
	     if !print_debug_por then
	       Printf.printf "After IN. Lists' sizes: %d,%d.\n"
			     (List.length !left_input_set)
			     (List.length !right_input_set);

	     (* ** Fourth Step/IN : apply the internal transitions (including conditionals) *)  
	     let left_in_internal = ref []
	     and right_in_internal = ref [] in

	     (* Scan all symbolic processes, flatten all parallels/choices and perform all available
   conditionals and branch  for then/else and for the different ways (disjunction)
   to satisfy the  conditional's test. Thanks to our "function_next", we then put
   all those alternatives together in left/right_internal lists. *)
	     List.iter (fun symb_proc_1 ->
			Process.apply_internal_transition
			  false
			  true
			  (fun symb_proc_2 -> 
			   let simplified_symb_proc = Process.simplify symb_proc_2 in
			   if not (Process.is_bottom simplified_symb_proc)
			   then left_in_internal := simplified_symb_proc :: !left_in_internal
			  ) symb_proc_1
		       ) !left_input_set;
	     
	     List.iter (fun symb_proc_1 ->
			Process.apply_internal_transition
			  false
			  true
			  (fun symb_proc_2 -> 
			   let simplified_symb_proc = Process.simplify symb_proc_2 in
			   if not (Process.is_bottom simplified_symb_proc)
			   then right_in_internal := simplified_symb_proc :: !right_in_internal
			  ) symb_proc_1
		       ) !right_input_set;
	     
	     if !print_debug_por then
	       Printf.printf "After IN+TEST. Lists' sizes: %d,%d.\n"
			     (List.length !left_in_internal)
			     (List.length !right_in_internal);

	     (* Same explanations as above (for OUT) *)
	     if (!left_in_internal <> []) || ( !right_in_internal <> [])
	     then next_function_input !left_in_internal !right_in_internal;
	   end;
       end	     
       | false, false ->
       (***************** WITHOUT FOCUS ****************************  *)
	  begin
	    
	    let left_output_set = ref []
	    and right_output_set = ref [] in
	    
	    let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support in
	    
	    (* Idée: on va chercher le canal du premier output qu'on trouve parmi
   les processus DES left_internal. Si il y en a pas alors on est positif
   à gauche: dans ce cas on check qu'on l'est à droite et on passe en mode input.
   Sinon, on récupére le canal de cet output et on applique les List.iter
   pour CET output/canal. A CHECK: label, channel en mode Term.term suffisant?
   ETC... *)
	    
	    Process.apply_output
	      true
	      (fun (symb_proc_2,_) -> 
	       (* We do not simplify symbolic processes because it will be done
	  in the next step when performing conditionals/splittings. *)
	       left_output_set := symb_proc_2::!left_output_set)
	      var_r_ch
	      proc_left_label;

	    Process.apply_output
	      true
	      (fun (symb_proc_2,_) -> 
	       (* same as above *)
	       right_output_set := symb_proc_2::!right_output_set)
	      var_r_ch
	      proc_right_label;
	    
	    if !print_debug_por then
	      Printf.printf "After OUT. Lists' sizes: %d,%d.\n"
    			    (List.length !left_output_set)
    			    (List.length !right_output_set);


	    (* ** Third Step : apply the internal transitions (including conditionals) *)  
	    let left_out_internal = ref []
	    and right_out_internal = ref [] in

	    (* Scan all symbolic processes, flatten all parallels/choices and perform all available
   conditionals and branch  for then/else and for the different ways (disjunction)
   to satisfy the  conditional's test. Thanks to our "function_next", we then pu
   all those alternatives together in left/right_internal lists. *)
	    List.iter (fun symb_proc_1 ->
		       Process.apply_internal_transition
			 false
			 true
			 (fun symb_proc_2 -> 
			  let simplified_symb_proc = Process.simplify symb_proc_2 in
			  if not (Process.is_bottom simplified_symb_proc)
			  then left_out_internal := simplified_symb_proc :: !left_out_internal
			 ) symb_proc_1
		      ) !left_output_set;
	    
	    List.iter (fun symb_proc_1 ->
		       Process.apply_internal_transition
			 false
			 true (fun symb_proc_2 -> 
			       let simplified_symb_proc = Process.simplify symb_proc_2 in
			       if not (Process.is_bottom simplified_symb_proc)
			       then right_out_internal := simplified_symb_proc::!right_out_internal
			      ) symb_proc_1
		      ) !right_output_set;
	    

	    (* We pass those alternatives to the next step which consists in:
     1. put all csys in a row matrix
     2. apply Strategy.apply_strategy_input/output resulting in a branching
        process ending with many matrices (for leaves: in solved form)
     3. apply final_test_on_matrix on all those leaves, if OK:
     4. apply partitionate_matrix giving many pairs of symbolic processes
     5. recursive calls on each of them
	     *)

	    if !print_debug_por then
	      Printf.printf "After OUT+TEST. Lists' sizes: %d,%d.\n"
			    (List.length !left_out_internal)
			    (List.length !right_out_internal);

	    if !left_out_internal <> [] || !right_out_internal <> []
	    then next_function_output !left_out_internal !right_out_internal;
	  end

	 

(* EEEEE ************************************************** *)
(*  ************************************************** *)


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
			     
  in let strategy_one_transition = if !option_por
				   then apply_strategy_one_transition_por
				   else apply_strategy_one_transition
     and next_out = next_function
		      (if false (* !option_por*)
		       then Strategy.apply_full_strategy 
		       else Strategy.apply_strategy_output)
     and next_in = next_function
		     (if false (* !option_por *)
		      then Strategy.apply_full_strategy 
		      else Strategy.apply_strategy_input)
     in
     strategy_one_transition 
       next_out
       next_in
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
