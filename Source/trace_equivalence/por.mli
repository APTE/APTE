(** This module interfaces POR with Apte *)

(** Abstract type for sets of symbolic traces to be explored *)
type trs
type actionA = In of Standard_library.Term.term | Out of Standard_library.Term.term
							   
(** Returns true when a given term representing a IN/Out(channel) in Apte is enable in the given set of traces. *)
val isEnable : actionA -> trs -> bool
				   
(** [importProcess p] gives the LTS representation of a process [p]*)
val importProcess : Standard_library.Process.process -> Porridge.Process_.t

(** [importProcess lp1 lp2] compute the reduced set of traces to be explored associated to [[p1;0 || p2;0]]. *)
val computeTraces : Porridge.Process_.t -> Porridge.Process_.t -> trs

								  
