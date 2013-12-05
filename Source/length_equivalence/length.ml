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

let names_length = ref 10.0

let length_functions = ref []


let complete_length_functions () = 
  let symb_l = List.filter (fun f -> not (List.mem_assq f !length_functions)) !Term.all_constructors in
  
  let rec create_list = function
    | 0 -> []
    | n -> 1.0 :: (create_list (n-1))
  in
  
  List.iter (fun f -> 
    length_functions := (f,(1.0,create_list (Term.get_arity f)))::!length_functions
  ) symb_l
    
(* We assume the polynom is sorted alphabetically *)
type polynomial = ((int list) * float) list

let rec display_list = function
  | [] -> ""
  | i::q -> (string_of_int i)^"*"^(display_list q)

let display_polynomial = 
  List.iter (fun (param,coeff) -> 
    Printf.printf "{ var = %s, coeff = %f } + " (display_list param) coeff
  )
  
(*******************)

(** Returns:
  - 0 if equal
  - -1 if p1 < p2
  - 1 if p1 > p2 *)
let rec comp_param_l p1 p2 = match p1,p2 with
  | [],[] -> 0
  | [],_ -> -1
  | _,[] -> 1
  | i1::q1, i2::q2 when i1 = i2 -> comp_param_l q1 q2
  | i1::_,i2::_ when i1 < i2 -> -1
  | _,_ -> 1

let rec addition_polynoms poly1 poly2 = match poly1,poly2 with
  | [],_ -> poly2
  | _,[] -> poly1
  | (param_l1,coeff1)::q1, (param_l2,coeff2)::q2 ->      
      begin match comp_param_l param_l1 param_l2 with
        | 0 -> (param_l1,coeff1 +. coeff2)::(addition_polynoms q1 q2)
        | 1 -> (param_l1,coeff1)::(addition_polynoms q1 poly2)
        | _ -> (param_l2,coeff2)::(addition_polynoms poly1 q2)
      end
      
let rec multiply_polynom_by_coeff coeff poly = match poly with
  | [] -> []
  | (l,c)::q -> (l,c *. coeff)::(multiply_polynom_by_coeff coeff q)

let rec polynom_of_term term assoc_var_param= 
  if Term.is_name term 
  then [([],!names_length)]
  else if Term.is_variable term 
  then 
    let param_x = List.assq (Term.variable_of_term term) assoc_var_param in
    [([param_x],1.0)]
  else
    let symb = Term.top term in
    let (cst_coeff,args_coeff) = List.assq symb !length_functions in
    
    let cst_poly = [([],cst_coeff)] in
    
    Term.fold_left_args2 (fun poly_acc arg coeff ->
      let poly = multiply_polynom_by_coeff coeff (polynom_of_term arg assoc_var_param) in
      addition_polynoms  poly poly_acc
    ) cst_poly term args_coeff
      
    
let rec is_equal_polynomial poly1 poly2 = match poly1,poly2 with
  | [],[] -> true
  | [],_ -> false
  | _,[] -> false
  | (param_l1,coeff1)::q1, (param_l2,coeff2)::q2 when ((comp_param_l param_l1 param_l2) = 0) && coeff1 = coeff2 -> is_equal_polynomial q1 q2
  | _,_ -> false
  
let polynomial_of_constraint_system csys = 
  let assoc = ref [] in
  let acc = ref 1 in
  
  Constraint.iter Constraint.SAll (fun dc ->
    let x = Term.variable_of_term (Constraint.Deducibility.get_message dc) in
    assoc := (x,!acc):: !assoc;
    incr acc
  ) (Constraint_system.get_deducibility_constraint_set csys);
  
  let poly_frame = ref [] in
  
  Constraint.iter Constraint.SAll (fun fc ->
    poly_frame := (polynom_of_term (Constraint.Frame.get_message fc) !assoc) :: !poly_frame
  ) (Constraint_system.get_frame csys);
  
  !poly_frame
      
