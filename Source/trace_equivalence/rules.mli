(*************************************************************************
** APTE v0.4beta - Algorithm for Proving Trace Equivalence              **
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

(** Definitions of the rules *)

open Standard_library

(** This module regroups all the functions that describes the application of rules on constraint systems. *)

(** {2 Rule {% \Cons %} } *)

val apply_cons_row_matrix :
  Recipe.variable ->
  Term.symbol -> 
  Constraint_system.row_matrix ->
  Constraint_system.row_matrix option * Constraint_system.row_matrix option
  
val apply_external_cons_phase_1 :
  Recipe.variable ->
  Term.symbol ->
  Constraint_system.matrix ->
  Constraint_system.matrix * Constraint_system.matrix
  
val apply_external_cons_phase_2 :
  Recipe.variable ->
  Term.symbol ->
  Constraint_system.matrix ->
  Constraint_system.matrix * Constraint_system.matrix
  
(** {2 Rule {% \Axiom %} } *)  
  
val apply_axiom_row_matrix :
  int ->
  Recipe.variable ->
  Recipe.path -> 
  Constraint_system.row_matrix ->
  Constraint_system.row_matrix option * Constraint_system.row_matrix option
  
val apply_external_axiom_phase_1 :
  int ->
  Recipe.variable ->
  Recipe.path -> 
  Constraint_system.matrix ->
  Constraint_system.matrix * Constraint_system.matrix
  
val apply_external_axiom_phase_2 :
  int ->
  Recipe.variable ->
  Recipe.path -> 
  Constraint_system.matrix ->
  Constraint_system.matrix * Constraint_system.matrix
  
(** {2 Rule {% \Dest %} } *)  

val apply_full_column_dest :
  int -> 
  Recipe.path ->
  int ->
  Term.symbol ->
  Constraint_system.matrix ->
  Constraint_system.matrix * (Recipe.path * Recipe.variable) list
  
val apply_full_column_dest_tuple :
  int -> 
  Recipe.path ->
  Term.symbol ->
  Constraint_system.matrix ->
  Constraint_system.matrix * (Recipe.path * Recipe.variable) list
  
(** {2 Rule {% \Eqll %} } *)

val apply_eqll : int -> Constraint_system.matrix -> Constraint_system.matrix

(** {2 Rule {% \Eqlr %} } *)

val apply_full_column_eqlr : 
  int -> 
  Recipe.path ->
  Recipe.variable ->
  Constraint_system.matrix ->
  Constraint_system.matrix
  
val apply_full_column_eqlr_frame :
  int -> 
  Recipe.path ->
  int ->
  Recipe.path ->
  Constraint_system.matrix ->
  Constraint_system.matrix
  
(** {2 Rule {% \Eqrr %} } *)

val apply_eqrr_row_matrix : 
  Recipe.variable -> Recipe.variable -> 
  Constraint_system.row_matrix ->
  Constraint_system.row_matrix option * Constraint_system.row_matrix option
  
val apply_external_eqrr_phase_1 : 
  Recipe.variable -> Recipe.variable -> 
  Constraint_system.matrix ->
  Constraint_system.matrix * Constraint_system.matrix
  
val apply_external_eqrr_phase_2 :
  Recipe.variable -> Recipe.recipe ->
  Constraint_system.matrix ->
  Constraint_system.matrix * Constraint_system.matrix
  
(** {2 Rule {% \DedSubterms %} } *)

val apply_dedsubterm_row_matrix :
  Recipe.path ->
  int ->
  Term.symbol ->
  int ->
  Constraint_system.row_matrix ->
  Constraint_system.row_matrix option * Constraint_system.row_matrix option
