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

open Standard_library

exception Not_equivalent_left of Process.symbolic_process
exception Not_equivalent_right of Process.symbolic_process

let final_test_count = ref 0
let number_of_branches_cut = ref 0
let size_trace_cutted = Array.make 100 0

let display_size_trace_cutted ()= 
  let result = ref "" in
  for i = 0 to 99 do
    if size_trace_cutted.(i) <> 0
    then result := Printf.sprintf "%s, %d size %d" !result size_trace_cutted.(i) i 
  done;
  !result
  
(** [final_test_on_matrix] does nothing if the final test succeed otherwise it raise one of the two exception [Not_equivalent_left] and [Not_equivalent_right] *)
let final_test_on_matrix index_right_process left_set right_set matrix = 
  final_test_count := !final_test_count + 1;
  if (!final_test_count / 1000) * 1000 = !final_test_count then 
    begin
      Printf.printf "Final test reached = %d, number_of_branches_cut = %d (%s)\n" !final_test_count !number_of_branches_cut (display_size_trace_cutted ());
      flush_all ()
    end;

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
        then 
          (* Length test *)
          begin 
          let _,poly_left, poly_right = Constraint_system.Matrix.fold_left_on_row i (fun (k,left_poly,right_poly) csys ->
            if Constraint_system.is_bottom csys
            then (k+1,left_poly, right_poly)
            else if k < index_right_process
            then 
              let poly = Length.polynomial_of_constraint_system csys in
              (k+1, (poly,(csys,k))::left_poly , right_poly)
            else
              let poly = Length.polynomial_of_constraint_system csys in
              (k+1, left_poly,(poly,(csys,k))::right_poly)
            ) (1,[],[]) matrix
          in
          
          let equal_seq_poly (pl1,_) (pl2,_) =
            List.for_all2 Length.is_equal_polynomial pl1 pl2
          in
          
          begin try
            let (_,(csys_attack,j_attack)) = List.find (fun p_left -> List.for_all (fun p_right -> not (equal_seq_poly p_left p_right)) poly_right) poly_left in
            let symb_proc = List.nth left_set (j_attack-1) in
            let symb_proc' = Process.replace_constraint_system csys_attack symb_proc in
            raise (Not_equivalent_left (symb_proc'))
          with Not_found -> 
            begin
              try
                let (_,(csys_attack,j_attack)) = List.find (fun p_right -> List.for_all (fun p_left -> not (equal_seq_poly p_left p_right)) poly_left) poly_right in
                let symb_proc = List.nth right_set (j_attack-index_right_process) in
                let symb_proc' = Process.replace_constraint_system csys_attack symb_proc in
                raise (Not_equivalent_right (symb_proc'))
              with Not_found -> ()
            end
          end
          end
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
  
(****** Partition of the matrix ******)
  
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
  
(** [partionate_matrix] partionate a matrix that successfully passed [final_test_on_matrix] *)
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
    
  
let rec erase_double = function 
  | [] -> []
  | symb_csys::q when List.exists (Process.is_same_input_output symb_csys) q -> erase_double q
  | symb_csys::q -> symb_csys::(erase_double q)
  
let internal_communication = ref false

(** We assume that the constraint systems in the symbolic processes are in solved form *)
let rec apply_strategy support left_symb_proc_l right_symb_proc_l = 
  (* First step : apply the internal transitition *)
  (* We assume for the moment that the internal communication are applied *)  
  
  (* Erase double *)
  
  let left_symb_proc_list = erase_double left_symb_proc_l
  and right_symb_proc_list = erase_double right_symb_proc_l in
  
  let left_internal = ref []
  and right_internal = ref [] in
  
  List.iter (fun symb_proc_1 ->
    Process.apply_internal_transition !internal_communication (fun symb_proc_2 -> 
      left_internal := symb_proc_2::!left_internal
    ) symb_proc_1
  ) left_symb_proc_list;
  
  List.iter (fun symb_proc_1 ->
    Process.apply_internal_transition !internal_communication (fun symb_proc_2 -> 
      right_internal := symb_proc_2::!right_internal
    ) symb_proc_1
  ) right_symb_proc_list;
  
  let left_set = ref []
  and right_set = ref [] in
  
  let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support in
  
  List.iter (fun symb_proc_1 ->
    Process.apply_output (fun symb_proc_2 -> 
      (* At this point the constraint system in the symbolic_process are not simplified *)
      let simplified_symb_proc = Process.simplify symb_proc_2 in
      if not (Process.is_bottom simplified_symb_proc)
      then left_set := simplified_symb_proc::!left_set
    ) var_r_ch symb_proc_1
  ) !left_internal;
  
  List.iter (fun symb_proc_1 ->
    Process.apply_output (fun symb_proc_2 -> 
      (* At this point the constraint system in the symbolic_process are not simplified *)
      let simplified_symb_proc = Process.simplify symb_proc_2 in
      if not (Process.is_bottom simplified_symb_proc)
      then right_set := simplified_symb_proc::!right_set
    ) var_r_ch symb_proc_1
  ) !right_internal;
  
  (* Third step : apply the strategy on the output matrix *)
  
  if !left_set <> [] || !right_set <> []
  then apply_strategy_for_constraint_system Trace_equivalence.Strategy.apply_strategy_output !left_set !right_set;
  
  (* Fourth step : apply the input transition *)
  
  left_set := [];
  right_set := [];
  
  let var_r_ch = Recipe.fresh_free_variable_from_id "Z" support
  and var_r_t = Recipe.fresh_free_variable_from_id "Y" support in
  
  List.iter (fun symb_proc_1 ->
    Process.apply_input (fun symb_proc_2 -> 
      (* At this point the constraint system in the symbolic_process are not simplified *)
      let simplified_symb_proc = Process.simplify symb_proc_2 in
      if not (Process.is_bottom simplified_symb_proc)
      then left_set := simplified_symb_proc::!left_set
    ) var_r_ch var_r_t symb_proc_1
  ) !left_internal;
  
  List.iter (fun symb_proc_1 ->
    Process.apply_input (fun symb_proc_2 -> 
      (* At this point the constraint system in the symbolic_process are not simplified *)
      let simplified_symb_proc = Process.simplify symb_proc_2 in
      if not (Process.is_bottom simplified_symb_proc)
      then right_set := simplified_symb_proc::!right_set
    ) var_r_ch var_r_t symb_proc_1
  ) !right_internal;
  
  (* Fifth step : apply the strategy on the input matrix *)
  
  if !left_set <> [] || !right_set <> []
  then apply_strategy_for_constraint_system Trace_equivalence.Strategy.apply_strategy_input !left_set !right_set
  
  
and apply_strategy_for_constraint_system f_csys_strategy left_set right_set = 
  (* First step : Creation of the matrix of constraint system and application  *)
    
  let number_left_symb_proc = List.length left_set
  and number_right_symb_proc = List.length right_set in
     
  let index_right_process = number_left_symb_proc + 1 in
  
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
 
  let support = Constraint_system.Matrix.get_maximal_support matrix in
  
  (* Second step : Application of the strategy *)
  
  f_csys_strategy (fun matrix_1 ->
    if Constraint_system.Matrix.is_empty matrix_1
    then ()
    else
      begin
        final_test_on_matrix index_right_process left_set right_set matrix_1;
        partionate_matrix (fun left_symb_proc_l right_symb_proc_l ->
          apply_strategy support left_symb_proc_l right_symb_proc_l
        ) left_set right_set index_right_process matrix_1
      end
  ) matrix  
  
  
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
  
  let nb_free_names = List.length free_names in
  
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
    apply_strategy nb_free_names [symb_proc1] [symb_proc2];
    true
  with
    | Not_equivalent_left sym_proc ->
        Printf.printf "Witness of non-equivalence on Process 1:\n%s"
          (Process.display_trace sym_proc);
        false
    | Not_equivalent_right sym_proc ->
        Printf.printf "Witness of non-equivalence on Process 2:\n%s"
          (Process.display_trace sym_proc);
        false
  
  
  
