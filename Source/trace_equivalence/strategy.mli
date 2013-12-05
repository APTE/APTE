open Standard_library

val apply_strategy_input : 
  (Constraint_system.matrix -> unit) -> 
  Constraint_system.matrix ->
  unit
  
val apply_strategy_output : 
  (Constraint_system.matrix -> unit) -> 
  Constraint_system.matrix ->
  unit
  
val apply_full_strategy :
  (Constraint_system.matrix -> unit) -> Constraint_system.matrix -> unit
