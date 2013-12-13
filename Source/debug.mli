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
