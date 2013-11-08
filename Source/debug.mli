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

(*******************************
***    Debuging function     ***
********************************)

exception Internal_error

(** [internal_error s] displays the error message [s] plus some other information. 
    @raise Failure. *)
val internal_error : string -> 'a

(** Type [debug_mode] denotes the kind of debugging that will be done during the execution
    of the algorithm. *)
type debug_mode = 
  | High (** All debug function will be applied. It includes some heavy verification that may impact on the time of execution. *)
  | Low  (** By default. The basic verification. The impact on time execution "should" not be visible. *)
  | None (** No debugging at all. Use this option when time execution is a critical matter. *)

(** Initialisation of the debug function. *)
val initialise_debugging : debug_mode -> unit

(** [high_debugging f] executes [f] if the function [initialise_debugging] was previously called with debug mode [High], else it does nothing. *)
val high_debugging : (unit -> unit) -> unit

(** [low_debugging f] executes [f] if the function [initialise_debugging] was previously called with debug mode [High] or [Low], else it does nothing. *)
val low_debugging : (unit -> unit) -> unit

val display_debug : string -> unit
