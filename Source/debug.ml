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
***    Debuging function     ***
********************************)

exception Internal_error

type debug_mode = 
  | High
  | Low
  | None

let internal_error msg = 
  Printf.printf "Internal error : %s\nPlease report the bug to cheval@lsv.ens-cachan.fr with the input file and output\n" msg;
  raise Internal_error
  
let high_debug_function = ref (fun _ -> ())

let low_debug_function = ref (fun f -> f ())

let initialise_debugging = function
  | High -> 
      high_debug_function := fun f -> f ();
      low_debug_function := fun f -> f ()
  | Low ->
      high_debug_function := fun _ -> ();
      low_debug_function := fun f -> f ()
  | None ->
      high_debug_function := fun _ -> ();
      low_debug_function := fun _ -> ()

  
let high_debugging f = !high_debug_function f

let low_debugging f = !low_debug_function f
(*
let display_debug str =
  Printf.printf "%s\n" str;
  flush_all ()*)
  
let display_debug _ = ()
