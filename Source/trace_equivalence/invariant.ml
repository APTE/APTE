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

let invVarConstraint s csys =
  if Constraint_system.is_bottom csys
  then true
  else
    not (Constraint.exists (Constraint.SUntil s) (fun dc -> 
      not (Term.is_variable (Constraint.Deducibility.get_message dc)) ||
      (Constraint.exists Constraint.SAll (fun dc' ->
         not (Recipe.is_equal_variable (Constraint.Deducibility.get_recipe_variable dc) (Constraint.Deducibility.get_recipe_variable dc')) && (Term.is_equal_term (Constraint.Deducibility.get_message dc) (Constraint.Deducibility.get_message dc'))
      ) (Constraint_system.get_deducibility_constraint_set csys))
    ) (Constraint_system.get_deducibility_constraint_set csys))
    
let invPPunSb support column matrix =
  Constraint_system.Matrix.check_structure matrix;
  
  for i = 1 to column do
    if Constraint_system.Matrix.exists_in_col i (fun csys -> not (invVarConstraint support csys)) matrix
    then Debug.internal_error "[invariant.ml >> invPPunSb] The invariant InVarConstraint is not satisfied"
  done
  
