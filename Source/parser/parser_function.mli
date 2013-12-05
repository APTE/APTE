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

(***********************************
***            Types             ***
************************************)

type ident = string * int

type term =
  | Id of ident
  | FuncApp of ident * term list
  | Tuple of term list
  | Proj of int * int * term * int
  
type pattern = 
  | PVar of ident
  | PTuple of pattern list
  
type formula = 
  | Eq of term * term
  | Neq of term * term
  | And of formula * formula
  | Or of formula * formula
    
type process =
  | Call of ident * term list
  | Nil
  | Choice of process * process
  | Par of process * process
  | New of ident * process
  | In of term * ident * process
  | Out of term * term * process
  | Let of pattern * term * process
  | IfThenElse of formula * process * process
  
type declaration = 
  | ProcDecl of ident * ident list * process
  | FuncDecl of ident * int
  | LengthDecl of ident * (float * float list)
  | LengthTupleDecl of int * (float * float list) * int
  | FreeNameDecl of ident
  | Equivalence of process * process
  | EquivalenceLength of process * process
  
(***********************************
***            Parsing           ***
************************************)

val error_message : int -> string -> 'a

val initialise_environment : unit -> unit

val equivalence_request : (Process.process * Process.process) list ref

val equivalence_length_request : (Process.process * Process.process) list ref

val parse_one_declaration : declaration -> unit
