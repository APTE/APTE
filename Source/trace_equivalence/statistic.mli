(*******************************
***    Statistic function     ***
********************************)

open Standard_library

type statistic_mode =
  | Final
  | Periodic of int
  | None

type step_strategy =
  | POne_SA | POne_SB | POne_SC | POne_SD | POne_SE
  | PTwo_SA | PTwo_SB | PTwo_SC
  | Leaf

val initialise_statistic : statistic_mode -> unit

val initialise_log : int -> unit

val reset_statistic : unit -> out_channel

(** Records functions *)

val record_matrix : step_strategy -> Constraint_system.matrix -> unit

val start_transition : Process.symbolic_process list -> Process.symbolic_process list -> unit

val end_transition : unit -> unit

(** Display function *)

type display_mode =
  | Global
  | Per_size
  | Per_step

val initialise_display : display_mode -> unit

val display_statistic : unit -> unit
