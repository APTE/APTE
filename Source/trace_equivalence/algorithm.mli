open Standard_library

exception Not_equivalent_left of Process.symbolic_process
exception Not_equivalent_right of Process.symbolic_process

(** Option for the algorithm *)

val option_semantics : Process.semantics ref

(** true if POR technique with compression will be used (only for action-determinate proc.) *)
val option_compr : bool ref

(** true if POR technique with compression plus reduction will be used (only for action-determinate proc.) *)
val option_red : bool ref

(** true if POR technique with killing improper traces will be used (only for action-determinate proc.) *)
val option_improper : bool ref

(** true if POR technique with NoUse criterion (only for action-determinate proc.) *)
val option_nouse : bool ref

(** true if generalized POR technique will be used (no assumption on protocols given as inputs is required) *)
val option_por2 : bool ref

(*val option_internal_communication : bool ref*)

val option_erase_double : bool ref

val option_alternating_strategy : bool ref

(** true if we want to display all explored symbolic traces *)
val display_traces : bool ref

(** count the number of explored symbolic traces *)
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
  Term.name list ->
  (Process.symbolic_process list -> Process.symbolic_process list -> unit) ->
  (Process.symbolic_process list -> Process.symbolic_process list -> unit) ->
  (Process.symbolic_process list -> Process.symbolic_process list -> unit) ->
  Process.symbolic_process list ->
  Process.symbolic_process list ->
  unit

(** The strategy *)

val decide_trace_equivalence : Process.process -> Process.process -> bool

(** Debugging tools *)
val displayIfWitness : string -> Process.symbolic_process -> unit
val ifWitness : Process.symbolic_process -> bool

open Lpor
