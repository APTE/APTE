open Standard_library

val length_functions : (Term.symbol * (float * float list)) list ref

val complete_length_functions : unit -> unit

type polynomial

val polynomial_of_constraint_system : Constraint_system.constraint_system -> polynomial list

val is_equal_polynomial : polynomial -> polynomial -> bool

val display_polynomial : polynomial -> unit
