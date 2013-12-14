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

(** {2 Process} *)

type label

val fresh_label : unit -> label

type formula = 
  | Eq of Term.term * Term.term
  | Neq of Term.term * Term.term
  | And of formula * formula
  | Or of formula * formula
  
type pattern = 
  | Var of Term.variable
  | Tuple of Term.symbol * pattern list

type process =
  | Nil
  | Choice of process * process
  | Par of process * process
  | New of Term.name * process * label
  | In of Term.term * Term.variable * process * label
  | Out of Term.term * Term.term * process * label
  | Let of pattern * Term.term * process * label
  | IfThenElse of formula * process * process * label

val refresh_label : process -> process

val rename : process -> process

val iter_term_process : process -> (Term.term -> Term.term) -> process

val get_free_names : process -> Term.name list

val display_process : process -> string

(** {2 Symbolic process} *)

type symbolic_process 

val create_symbolic : (Recipe.recipe * Term.term) list -> process -> Constraint_system.constraint_system -> symbolic_process

val display_trace : symbolic_process -> string

val display_trace_no_unif : symbolic_process -> string

(** {4 Testing} *)

val is_bottom : symbolic_process -> bool

(** {4 Access and modification} *)

val get_constraint_system : symbolic_process -> Constraint_system.constraint_system

val replace_constraint_system : Constraint_system.constraint_system -> symbolic_process -> symbolic_process

val simplify : symbolic_process -> symbolic_process

val size_trace : symbolic_process -> int

val instanciate_trace : symbolic_process -> symbolic_process

(** {4 Transition application} *)

val apply_internal_transition : bool -> (symbolic_process -> unit) -> symbolic_process -> unit

val apply_input : (symbolic_process -> unit) -> Recipe.variable -> Recipe.variable -> symbolic_process -> unit

val apply_output : (symbolic_process -> unit) -> Recipe.variable -> symbolic_process -> unit

(** {3 Optimisation} *)

val is_same_input_output : symbolic_process -> symbolic_process -> bool
