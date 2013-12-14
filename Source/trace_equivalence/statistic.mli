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

(*******************************
***    Statistic function     ***
********************************)

open Standard_library

type statistic_mode =
  | Final
  | Periodic of int
  | None

type step_strategy = 
  | POne_SA | POne_SB | POne_SC | POne_SD | POne_SE 
  | PTwo_SA | PTwo_SB | PTwo_SC 
  | Leaf    

val initialise_statistic : statistic_mode -> unit

val initialise_log : int -> unit

val reset_statistic : unit -> out_channel

(** Records functions *)
  
val record_matrix : step_strategy -> Constraint_system.matrix -> unit

val start_transition : Process.symbolic_process list -> Process.symbolic_process list -> unit

val end_transition : unit -> unit

(** Display function *)

type display_mode = 
  | Global
  | Per_size
  | Per_step

val initialise_display : display_mode -> unit  
  
val display_statistic : unit -> unit
