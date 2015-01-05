open Standard_library

exception Not_equivalent_left of Process.symbolic_process
exception Not_equivalent_right of Process.symbolic_process

(** Option for the algorithm *)

(** true if POR technique with compression will be used (only for action-determinate proc.) *)
val option_compr : bool ref

(** true if POR technique with compression plus reduction will be used (only for action-determinate proc.) *)
val option_red : bool ref

(** true if POR technique with killing improper traces will be used (only for action-determinate proc.) *)
val option_improper : bool ref

val option_internal_communication : bool ref

val option_erase_double : bool ref

val option_alternating_strategy : bool ref

val display_traces : bool ref

val final_test_count : int ref

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




