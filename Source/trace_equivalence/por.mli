(** This module interfaces POR with Apte *)

(** [importTerm t] gives the LTS representation of a term [t] *)
val importTerm : Standard_library.Term.term -> Term_.t

(** [importProcess p] gives the LTS representation of a process [p]*)
val importProcess : Standard_library.Process.process -> Porridge.Process.t

(** [importProcess lp1 lp2] compute the reduced set of traces to be explored associated to [[p1;0 || p2;0]]. *)
val computeTraces : Porridge.Process.t -> Porridge.Process.t -> unit
