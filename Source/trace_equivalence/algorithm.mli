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

open Standard_library

exception Not_equivalent_left of Process.symbolic_process
exception Not_equivalent_right of Process.symbolic_process

(** Option for the algorithm *)

val option_internal_communication : bool ref

val option_erase_double : bool ref

val option_alternating_strategy : bool ref

(** Functions for the strategy *)

val partionate_matrix :
  (Process.symbolic_process list -> Process.symbolic_process list -> unit) ->
  Process.symbolic_process list ->
  Process.symbolic_process list ->
  int ->
  Constraint_system.matrix ->
  unit

val apply_strategy_for_matrices :
  (int -> Constraint_system.matrix -> unit) ->
  ((Constraint_system.matrix -> unit) -> Constraint_system.matrix -> unit) ->
  Process.symbolic_process list ->
  Process.symbolic_process list ->
  unit
  
val apply_strategy_one_transition  :
  (Process.symbolic_process list -> Process.symbolic_process list -> unit) ->
  (Process.symbolic_process list -> Process.symbolic_process list -> unit) ->
  Process.symbolic_process list ->
  Process.symbolic_process list ->
  unit
  
(** The strategy *)  

val decide_trace_equivalence : Process.process -> Process.process -> bool




