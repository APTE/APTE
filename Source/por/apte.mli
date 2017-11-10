(** This module interfaces POR with Apte *)

(** [import t] gives the LTS representation of a process [t] *)
val import : Standard_library.Process.process -> Process.process
