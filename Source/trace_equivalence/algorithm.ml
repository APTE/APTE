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

(* for the release, set booleans to false, true, true, true *)
(* for POR, set booleans to true, false, false, false *)
let option_compr = ref false

let option_red = ref false

let option_nouse = ref false

let option_improper = ref false

let option_internal_communication = ref true

let option_erase_double = ref true

let option_alternating_strategy = ref true
  
let print_debug_por = ref false

let display_traces = ref false

(** Statistics info *)
let final_test_count = ref 0


(************************************
***    Debugging tools            ***
*************************************)

let pp = Printf.printf
(* use the following function to print all information about symP when its trace
   match the given witness. Write your witness as a list of integers (one integer
   per action accordingly to how Apte numbers/parses processes. *)
let witness = [2;3;4;27;28] 		(* example of witness *)
let size = 4				(* example of the 'looking' size *)
let displayIfWitness message symP =
  if (Process.is_subtrace witness size symP)
  then begin
      Printf.printf "%s%s" message (Process.display_trace symP);
      Printf.printf "%s" (Process.display_trace_blocks symP);
      Printf.printf "Here are its dependency constraints: %s" (Process.display_dep_csts symP);
      Process.display_symb_process symP;
      Printf.printf "%!\n";
    end
let ifWitness symP = (Process.is_subtrace witness size symP)


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
  (* we count the number of calls of this function (= nb. of final tests = nb. explorations) *)
  incr(final_test_count);

  if !display_traces && (left_symb_proc_list <> [] || right_symb_proc_list <> [])
  then (let one_proc = if left_symb_proc_list <> []
		       then List.hd left_symb_proc_list
		       else List.hd right_symb_proc_list in
	Printf.printf "%s\n" (Process.display_trace_simple one_proc));

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
    Process.apply_internal_transition !option_internal_communication !option_compr !option_improper (fun symb_proc_2 -> 
      left_internal := symb_proc_2::!left_internal
    ) symb_proc_1
  ) left_erase_set;
  
  List.iter (fun symb_proc_1 ->
    Process.apply_internal_transition !option_internal_communication !option_compr !option_improper (fun symb_proc_2 -> 
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
	       !option_compr (fun (symb_proc_2,_) -> 
			    let simplified_symb_proc = Process.simplify symb_proc_2 in
			    if not (Process.is_bottom simplified_symb_proc)
			    then left_output_set := simplified_symb_proc::!left_output_set
			   ) var_r_ch symb_proc_1
	    ) !left_internal;
  
  List.iter (fun symb_proc_1 ->
	     Process.apply_output
	       !option_compr (fun (symb_proc_2,_) -> 
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
	       !option_compr (fun (symb_proc_2,_) -> 
			    let simplified_symb_proc = Process.simplify symb_proc_2 in
			    if not (Process.is_bottom simplified_symb_proc)
			    then left_input_set := simplified_symb_proc::!left_input_set
			   ) var_r_ch var_r_t symb_proc_1
	    ) !left_internal;
  
  List.iter (fun symb_proc_1 ->
	     Process.apply_input
	       !option_compr (fun (symb_proc_2,_) -> 
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
(** Handles exceptions that Process.* may raise and raises
 the corresponding algorithm.ml exception *)
let try_P symproc_left symproc_right expr = 
  try 
    Lazy.force expr
  with
  | Process.Not_eq_left s ->
     begin
       Printf.printf "\nWitness' type: %s\n" s;
       Printf.printf "%s\n" (Process.display_trace_no_unif symproc_left);
       raise (Not_equivalent_left symproc_left);
     end
  | Process.Not_eq_right s ->
     begin
       Printf.printf "\nWitness' type: %s\n" s;
       Printf.printf "%s\n" (Process.display_trace_no_unif symproc_right);
       raise (Not_equivalent_right symproc_right);
     end
  | _ -> Debug.internal_error "[algorithm.ml >> tryP] We failed to handle an error in process.ml."
  

let apply_input_on_focused next_function_input proc_left_label proc_right_label = 
  (* The first process of proc_left/right_label is under focus. We perform
          their first input in case thay have the same skeleton and raise an exception otherwise.
	  We assume here that focus have been removed as soon as a negative pop out.*)

  let sk_left, sk_right = (Process.sk_of_symp proc_left_label, Process.sk_of_symp proc_right_label) in
  if not(Process.equal_skeleton sk_left sk_right)
  then begin
      Printf.printf "Witness' type: process under focus on the right does not match the one on the left.";
      Printf.printf "%s" ("Skeleton on the left: "^(Process.display_sk sk_left)^" and skeleton on the right: "^(Process.display_sk sk_right)^".\n");
      raise (Not_equivalent_right proc_right_label);
    end else
    begin
      let support = Constraint_system.get_maximal_support (Process.get_constraint_system proc_left_label) in
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

      (* We check that sets of processes are singletons *)
      if List.length !left_input_set != 1 || List.length !right_input_set != 1
      then Debug.internal_error "[algorithm.ml >> apply_input_on_focused] The sets of pocesses are not singletons. Should not happen since we have already check that skeletons match.";

      (* ** Apply the internal transitions (including conditionals) *)  
      let left_in_internal = ref []
      and right_in_internal = ref [] in

      (* Scan all symbolic processes, flatten all parallels/choices and perform all available
   conditionals and branch  for then/else and for the different ways (disjunction)
   to satisfy the  conditional's test. Thanks to our "function_next", we then put
   all those alternatives together in left/right_internal lists. *)
      List.iter (fun symb_proc_1 ->
		 Process.apply_internal_transition
		   false	(* with comm *)
		   true		(* with por *)
		   !option_improper 
		   (fun symb_proc_2 -> 
		    let simplified_symb_proc = Process.simplify symb_proc_2 in
		    if not (Process.is_bottom simplified_symb_proc)
		    then left_in_internal := simplified_symb_proc :: !left_in_internal
		   ) symb_proc_1
		) !left_input_set;
      
      List.iter (fun symb_proc_1 ->
		 Process.apply_internal_transition
		   false	(* with comm *)
		   true		(* with por *)
		   !option_improper 
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

      (* We pass those alternatives to the next step which consists in:
     1. put all csys in a row matrix
     2. apply Strategy.apply_strategy_input/output resulting in a branching
        process ending with many matrices (for leaves: in solved form)
     3. apply final_test_on_matrix on all those leaves, if OK:
     4. apply partitionate_matrix giving many pairs of symbolic processes
     5. recursive calls on each of them
       *)

      if (!left_in_internal <> []) || ( !right_in_internal <> [])
      then next_function_input !left_in_internal !right_in_internal;
    end


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
    begin
      let proc_left = (List.hd left_symb_proc_list) in
      Printf.printf "\n################### Before starting apply_strategy_one. Size of lists: %d,%d. Trace's size: %d\n"
		    (List.length left_symb_proc_list)
		    (List.length right_symb_proc_list)
		    (Process.size_trace proc_left);
      Printf.printf "%s" (Process.display_trace proc_left);
      Printf.printf "%s" (Process.display_trace_blocks proc_left);
    end;
  
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
	if !print_debug_por then Printf.printf "Trace is empty, we thus apply internal_transition before starting.\n";
	let ps = ref [] in
	Process.apply_internal_transition
	  false			(* with_comm *)
	  true			(* with_por *)
	  !option_improper 
	  (fun symb_proc_2 -> ps := symb_proc_2 :: !ps)
	  (List.hd left_symb_proc_list);
	let qs = ref [] in
	Process.apply_internal_transition
	  false			(* with_comm *)
	  true			(* with_por *)
	  !option_improper 
	  (fun symb_proc_2 -> qs := symb_proc_2 :: !qs)
	  (List.hd right_symb_proc_list);
	if (List.length !ps != 1)|| (List.length !qs != 1)
	then Debug.internal_error "[algorithm.ml >> apply_strategy_one_transition_por] The sets of pocesses after reducing conditionals are not singletons. It may be the case that inputted processes start with conditionals at top level (which is forbidden)."
	else (List.hd !ps, List.hd !qs)
      end;
  in
  (* we count the number of calls of this function (= nb. of final tests = nb. explorations) *)
  incr(final_test_count);
  
  if !display_traces
  then Printf.printf "%s\n" (Process.display_trace_simple proc_left);

  (* We check whether processes are improper or not and continue only if they are not *)
  (* Note that, the flag is_improper might be set to true when applying 'apply_internal[...]_without_comm *)
  let is_improper_left, is_improper_right = Process.is_improper proc_left, Process.is_improper proc_right in
  if (is_improper_left && is_improper_right)
  (* one of the two proc is improper -> we stop the exploration at this point *)
  (* if there is a mismatch: we cannot conclude anything since we only under-approximate improper block
          (e.g. on In.(if False then 0 | if False then 0)) *)
  then ()
  else
    (* we keep exploring the semantics *)
    begin

      (* ** FIRST step: labelises processes and update 'has_focus': at this point, some new processes coming from 
        breaking a parallel composition are in the multiset. All those new processes come from a unique parallel
        composition, so we label them in an arbitrary order but consistently with right_symb_proc_list.
        Note that, by perfomring this step we also check skl(P)=skl(Q) except when P is a null process (represented
        as an empty list in APTE). *)
  let proc_left_label, how_to_label = try_P proc_left proc_right (lazy (Process.labelise proc_left)) in
  let proc_right_label = try_P proc_left proc_right (lazy (Process.labelise_consistently how_to_label proc_right)) in
  (* Updating 'has_focus' on the left : set to false if focused process is negative *)
  let proc_left_label_up =
    if Process.has_focus proc_left_label 
    then (match Process.sk_of_symp proc_left_label with
	  | Process.OutS t -> Process.set_focus false proc_left_label
	  | _ -> proc_left_label)
    else proc_left_label
  (* Updating 'has_focus' on the right : set to false if focused process is negative *)
  and proc_right_label_up =
    if Process.has_focus proc_right_label 
    then (match Process.sk_of_symp proc_right_label with
	  | Process.OutS t -> Process.set_focus false proc_right_label
	  | _ -> proc_right_label)
    else proc_right_label in 

  if !print_debug_por then Printf.printf "end of labelling process...\n";

  (* We now update the complete_inp flag *)
  let proc_left_label_up,proc_right_label_up =
    if !option_red && (not (Process.has_focus proc_left_label_up) && not (Process.block_complete_inp proc_left_label_up))
    (* no focus (no more input) and flag complete_inp not already set to true -> first time without focus -> 
         we cannot already generate dep csts but we must set complete_inp flag to true *)
    then begin
	(Process.block_set_complete_inp proc_left_label_up,
	 Process.block_set_complete_inp proc_right_label_up)
      end
    else (proc_left_label_up,proc_right_label_up ) in

  (* Using the flag has_generate_dep_csts, we know if this is the first time last block has at least one output
	   In this case, we must generate dependency constraints for the last inputs. *)
  let proc_left_label_red_up, proc_right_label_red_up =
    if !option_red && Process.must_generate_dep_csts proc_left_label_up
    then begin
	if !print_debug_por then Printf.printf "End of INs blocks, we are going to try to add dependency constraints .... \n";
	(* For the moment we use only the left part to generate and test dependency constraints *)
	(Process.generate_dependency_constraints proc_left_label_up,
	 proc_right_label_up)
      end
    else (proc_left_label_up, proc_right_label_up) in
  
  (* ** We check the last remaining case to ensure skl(P)=skl(Q) that is P=0 and Q <> 0
          (we already deal withthe symmetric case since P can perform an action). *)
  if Process.is_null proc_left_label && not(Process.is_null proc_right_label) 
  then  begin
      Printf.printf "Witness' type: Null process on the left, not on the right.\n";
      raise (Not_equivalent_right proc_right_label);
    end;

  (* We keep exploring actions from this point only if all dependency constraints hold or reduction is not enabled *)
  if not(!option_red) || Process.test_dependency_constraints proc_left_label_red_up !option_nouse then    

    (* ** SECOND step: Distinguish two cases whether pro_left/right_label have focus or not.
         (if they do not have the same status we raise an error. *)
    match (Process.has_focus proc_left_label_red_up, Process.has_focus proc_right_label_red_up) with
    | true, false -> begin
		     Printf.printf "Witness' type: Release focus on the left, not on the right.\n";
		     raise (Not_equivalent_right proc_right_label_red_up);
		   end

    | false, true -> begin
		     Printf.printf "Witness' type: Release focus on the right, not on the left.\n";
		     raise (Not_equivalent_left proc_left_label_red_up);
		   end
    | true, true ->
       (****************** WITH FOCUS ****************************  *)
       (* In that case, the first process of proc_left/right_label_red_up is under focus. We perform
          their first input in case thay have the same skeleton and raise an exception otherwise.
	  We assume here that focus have been removed as soon as a negative pop out.*)
       apply_input_on_focused next_function_input proc_left_label_red_up proc_right_label_red_up
    | false, false ->
       (***************** WITHOUT FOCUS ****************************  *)
       begin
	 let support = Constraint_system.get_maximal_support (Process.get_constraint_system proc_left_label_red_up) in
	 let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support in
	 
	 (* We look for the first negative process in proc_left_left. Case (i): there is a such process.
              In that case we perform its first action, store the corresponding channel and look for a
              corresponding process on the right.
	     Note that, since we check that sk(P)=sk(Q) on new processes when labelling
	     them, it is not necesseray to check this now.
             Case (ii): P is positive. We iter over the whole list of proc_left_label_red_up.process:
	     put the selected process under focus, set has_focus to true, try to do the same on the right
             and apply apply_input_on_focused.*)
	   
	   let left_output_set = ref [] in
	   Process.apply_output
	     true
	     (fun (symb_proc_2,c) -> 
	      (* We do not simplify symbolic processes because it will be done
	  in the next step when performing conditionals/splittings. *)
	    left_output_set := (symb_proc_2,c)::!left_output_set)
	   var_r_ch
	   proc_left_label_red_up;
	 
	 if List.length !left_output_set = 0
	 then begin
	     (****** START A POSITIVE PHASE*****)
	     (* we must choose any process, put it at the begining of the list, set has_focus
                   to true, find a process with same skeleton on the right, do the same
                   and perform this input *)
	     if !print_debug_por then Printf.printf "[REL] We are going to start a new positive phase and thus choose a focus.\n";

	       (* we build a list (P_i,ski) list of alternatives of choices of focused process with the corresponding
	      focused process' skeleton ski*)
	     let left_choose_focus = Process.list_of_choices_focus proc_left_label_red_up in
	     (* using the latter we build a list (P_i,Q_i) list where Q_i is a choice of focus such that sk(P_i)=sk(Q_i) *)
	     let listPairProc = Process.assemble_choices_focus left_choose_focus proc_right_label_red_up in
	     (* in listPairProc : (symbolic_process^2) list, we apply input_focus on each pair with a List.iter *)
	     List.iter (fun (p_left,p_right) ->
			apply_input_on_focused next_function_input p_left p_right)
		       listPairProc;
	   end
	 else if List.length !left_output_set != 1
	 then Debug.internal_error "[algorithm.ml >> apply_strategy_one_transition_por] In a negative phase, we end up with more than one alternatives after performing an output. This should not happen."
	 else begin
	     (****** NEGATIVE PHASE ***********)
	     (* Contuining the negative phase, perform the same output on the right *)
	     let (proc_left_out, ch) = List.hd !left_output_set
	     and right_output_set = ref [] in
	     Process.apply_output_filter (* this will filter applying output to (first) output on channel ch *)
	       ch
	       (fun symb_proc_2 -> 
		(* same as above *)
		right_output_set := symb_proc_2::!right_output_set)
	       var_r_ch
	       proc_right_label_red_up;
	     
	     if !print_debug_por then
	       Printf.printf "After OUT. Lists' sizes: %d,%d.\n"
    			     (List.length !left_output_set)
    			     (List.length !right_output_set);

	     if List.length !right_output_set = 0
	     then begin
		 Printf.printf "Witness' type: right process cannot execute an output that the left one can perform.";
		 Printf.printf "%s" (" Here is the channel of this output: "^(Term.display_term ch)^".\n");
		 Printf.printf "%s\n" (Process.display_trace_no_unif proc_left_out);
		 raise (Not_equivalent_left proc_left_label_red_up);
	       end
	     else if List.length !right_output_set != 1
	     then Debug.internal_error "[algorithm.ml >> apply_strategy_one_transition_por] In a negative phase, we end up with more than one alternatives after performing an output. This should not happen.";
	     
	     (* ** Third Step : apply the internal transitions (including conditionals) *)  
	     let proc_right_out = List.hd !right_output_set
	     and left_out_internal = ref []
	     and right_out_internal = ref [] in

	     (* Scan all symbolic processes, flatten all parallels/choices and perform all available
   conditionals and branch  for then/else and for the different ways (disjunction)
   to satisfy the  conditional's test. Thanks to our "function_next", we then pu
   all those alternatives together in left/right_internal lists. *)
	     Process.apply_internal_transition
	       false
	       true
	       !option_improper 
	       (fun symb_proc_2 -> 
		let simplified_symb_proc = Process.simplify symb_proc_2 in
		if not (Process.is_bottom simplified_symb_proc)
		then left_out_internal := simplified_symb_proc :: !left_out_internal
	       ) proc_left_out;
	     
	     Process.apply_internal_transition
	       false
	       true 
	       !option_improper 
	       (fun symb_proc_2 -> 
		let simplified_symb_proc = Process.simplify symb_proc_2 in
		if not (Process.is_bottom simplified_symb_proc)
		then right_out_internal := simplified_symb_proc::!right_out_internal
	       ) proc_right_out;
	     

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
	   end; 
       end;
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
			     
  in let strategy_one_transition = if !option_compr
				   then apply_strategy_one_transition_por
				   else apply_strategy_one_transition
     and next_out = next_function
		      (if !option_compr
		       then Strategy.apply_full_strategy 
		       else Strategy.apply_strategy_output)
     and next_in = next_function
		     (if !option_compr
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
    Printf.printf "Number of final tests: %d.\n" (!final_test_count);
    true
  with
  | Not_equivalent_left sym_proc ->
     Printf.printf "Witness of non-equivalence on Process 1:\n%s"
		   (Process.display_trace (Process.instanciate_trace sym_proc));
     Printf.printf "Number of final tests: %d.\n" (!final_test_count);
     false
  | Not_equivalent_right sym_proc ->
     Printf.printf "Witness of non-equivalence on Process 2:\n%s"
		   (Process.display_trace (Process.instanciate_trace sym_proc));
     Printf.printf "Number of final tests: %d.\n" (!final_test_count);
     false
