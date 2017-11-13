(** This module interfaces POR with Apte *)

(** [importTerm t] gives the LTS representation of a term [t] *)
val importTerm : Standard_library.Term.term -> Term_.t

(** [importProcess p] gives the LTS representation of a process [p]*)
val importProcess : Standard_library.Process.process -> Process_.t
