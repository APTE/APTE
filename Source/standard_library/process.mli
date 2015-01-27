(** {2 Process} *)

type label

val fresh_label : unit -> label

type formula = 
  | Eq of Term.term * Term.term
  | Neq of Term.term * Term.term
  | And of formula * formula
  | Or of formula * formula
  
type pattern = 
  | Var of Term.variable
  | Tuple of Term.symbol * pattern list

type process =
  | Nil
  | Choice of process * process
  | Par of process * process
  | New of Term.name * process * label
  | In of Term.term * Term.variable * process * label
  | Out of Term.term * Term.term * process * label
  | Let of pattern * Term.term * process * label
  | IfThenElse of formula * process * process * label

val refresh_label : process -> process

val rename : process -> process

val iter_term_process : process -> (Term.term -> Term.term) -> process

val get_free_names : process -> Term.name list

val display_process : process -> string

(** {2 Symbolic process} *)

type symbolic_process 

val create_symbolic : (Recipe.recipe * Term.term) list -> process -> Constraint_system.constraint_system -> symbolic_process

(** display all information contained in the trace *)
val display_trace : symbolic_process -> string

(** only display labels *)
val display_trace_simple : symbolic_process -> string

(** like display_trace but do not apply substitutions to recipes *)
val display_trace_no_unif : symbolic_process -> string

(** like display_trace_no_unif but do not display constraints *)
val display_trace_no_unif_no_csts : symbolic_process -> string

val display_trace_blocks :  symbolic_process -> string

(** {4 Testing} *)

val is_bottom : symbolic_process -> bool

(** {4 Access and modification} *)

val get_constraint_system : symbolic_process -> Constraint_system.constraint_system

val replace_constraint_system : Constraint_system.constraint_system -> symbolic_process -> symbolic_process

val simplify : symbolic_process -> symbolic_process

val size_trace : symbolic_process -> int

val instanciate_trace : symbolic_process -> symbolic_process

(** {4 Transition application} *)

(** This function recursively apply all available internal transitions. In case with_por is enabled, it updates
    has_focus and improper accordingly. Note that, it also stores the fact that processes coming from parallel compositions
    must be labelled into the 'toLabel' flag. Flags: booleans: with_comm, with_por, with_improper. *)
val apply_internal_transition : bool -> bool -> bool -> (symbolic_process -> unit) -> symbolic_process -> unit

(** boolean: with_por (if true we only consider the process under focus, ie, the first one in the list)),
    next_function takes the pair (continuation,channel of produced action) *)
val apply_input : bool -> ((symbolic_process*Term.term) -> unit) -> Recipe.variable -> Recipe.variable -> symbolic_process -> unit

(** boolean: with_por, next_function takes the pair (continuation,channel of produced action) *)
val apply_output : bool -> ((symbolic_process*Term.term) -> unit) -> Recipe.variable -> symbolic_process -> unit


(** {3 Optimisation} *)

val is_same_input_output : symbolic_process -> symbolic_process -> bool


(** {Annotated semantics} *)

(** Excpetions raised when a witness is found *)
exception Not_eq_left of string
exception Not_eq_right of string

(*** Skeletons ***)
(** Skeletons of actions (it takes a channel as an argument) *)
type skeleton =
  | InS of Term.term
  | OutS of Term.term

(** Map from skeletons to 'a *)
module MapS : (Map.S with type key = skeleton)

(** Return [Some sk(P)] or [None] if non-reduced *)
val sk : process -> skeleton option

(** Return [sk(P)] where [P] is the focused process of the given symbolic process *)
val sk_of_symp : symbolic_process -> skeleton

(** Are two skeletons equal *)
val equal_skeleton : skeleton -> skeleton -> bool

(** Display a skeleton *)
val display_sk : skeleton -> string

(*** Labels ***)
(** Labels denoting sequential dependencies *)
type par_label

(** Display a par_label *)
val display_parlab : par_label -> string

(** The label we use to labelise initial process *)
val init_par_label : par_label

(** Labelises non-labelled processes in a symbolic process and outputs a map sk->lab *)
val labelise : symbolic_process -> (symbolic_process * par_label MapS.t)

(** Labelises non-labelled processes in a symbolic process accordingly to a map sk->lab *)
val labelise_consistently : par_label MapS.t -> symbolic_process -> symbolic_process

(*** has_focus handlers ***)
(** Returns the 'has_focus' flag *)
val has_focus : symbolic_process -> bool

(** Returns the 'is_improper' flag *)
val is_improper : symbolic_process -> bool

(** Modify the 'has_focus' flag *)
val set_focus : bool -> symbolic_process -> symbolic_process


(** {Compressed semantics} *)

(** The first term is the channel used for filtering outputs actions, next_function takes the pair (continuation,channel of produced action. *)
val apply_output_filter : Term.term -> (symbolic_process -> unit) -> Recipe.variable -> symbolic_process -> unit

(** Return all the choices of focus with the corresponding skeleton of focused process. Resulting symbolic process have a focus. *)
val list_of_choices_focus : symbolic_process -> (symbolic_process * skeleton) list

(** Given a list of left symbolic processes with a focus with their corresponding skeleton  and a right process,
   it assembles the corresponding answers of the right process (or raises a non-equiv exception). *)
val assemble_choices_focus: (symbolic_process * skeleton) list -> symbolic_process -> (symbolic_process * symbolic_process) list


(** {Reduced semantics} *)

(**  [test_dependency_constraints symP usingNoUse] test whether dependency constraints of [symP] hold. If usingNoUse is true then we use
the NoUse criterion as well. *)
val test_dependency_constraints : symbolic_process -> bool -> bool

(**  [generate_dependency_constraints symP] add to the set of dependency constrants
     of symP the dependency constraint for the inputs of the last block of the trace.  *)
val generate_dependency_constraints : symbolic_process -> symbolic_process

(** [block_set_complete_inp symP] set the flag complete_inp to true (inputs of last block are complete). *)
val block_set_complete_inp : symbolic_process -> symbolic_process

(** [block_complete_inp symP] returns the flag complete_inp (true if inputs of last block are complete). *)
val block_complete_inp : symbolic_process -> bool

(** [must_generate_dep_csts symP] returns true if we must generate dependency constraints (it is the first time that the last block
    is of the form [In* : []] and focused process is not positive (i.e., starts with an input) *)
val must_generate_dep_csts : symbolic_process -> bool



(** {4 Debugging tools} *)
(** [is_subtrace traceinfo size symP] Returns true if symP has executed a trace whose the [size] first actions are
    the same as in [traceinfo] *)
val is_subtrace : int list -> int -> symbolic_process -> bool

val display_symb_process : symbolic_process -> unit

val display_dep_csts : symbolic_process -> string

