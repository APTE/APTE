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

val display_trace : symbolic_process -> string

val display_trace_no_unif : symbolic_process -> string

(** {4 Testing} *)

val is_bottom : symbolic_process -> bool

(** {4 Access and modification} *)

val get_constraint_system : symbolic_process -> Constraint_system.constraint_system

val replace_constraint_system : Constraint_system.constraint_system -> symbolic_process -> symbolic_process

val simplify : symbolic_process -> symbolic_process

val size_trace : symbolic_process -> int

val instanciate_trace : symbolic_process -> symbolic_process

(** {4 Transition application} *)

(** booleans: with_comm, with_por *)
val apply_internal_transition : bool -> bool -> (symbolic_process -> unit) -> symbolic_process -> unit

(** boolean: with_por (if true we only consider the process under focus, ie, the first one in the list)),
    next_function takes the pair (continuation,channel of produced action) *)
val apply_input : bool -> ((symbolic_process*Term.term) -> unit) -> Recipe.variable -> Recipe.variable -> symbolic_process -> unit

(** boolean: with_por, next_function takes the pair (continuation,channel of produced action) *)
val apply_output : bool -> ((symbolic_process*Term.term) -> unit) -> Recipe.variable -> symbolic_process -> unit

(** {3 Optimisation} *)

val is_same_input_output : symbolic_process -> symbolic_process -> bool

(* (\** Scan a list of processes and return the channel of its first output (if any) *\) *)
(* val first_output : symbolic_process -> Term.term option *)


(** {Annotated semantics} *)

(** Skeletons of actions *)
type skeleton =
  | InS of Term.term
  | OutS of Term.term

(** Labels denoting sequential dependencies *)
type par_label

(** The label we use to labelise initial process *)
val init_par_label : par_label

(** Map from skeletons to 'a *)
module MapS : (Map.S with type key = skeleton)

(** Return Some sk(P) or None if non-reduced *)
val sk : process -> skeleton option

(** Return sk(P) where P is the focused process of the given symbolic process *)
val sk_of_symp : symbolic_process -> skeleton

(** Labelises non-labelled processes in a symbolic process and outputs a list of association sk->lab *)
val labelise : symbolic_process -> (symbolic_process * par_label MapS.t)

(** Labelises non-labelled processes in a symbolic process accordingly to a list of association sk->lab *)
val labelise_consistently : par_label MapS.t -> symbolic_process -> symbolic_process

(** Returns tha 'has_focus' flag *)
val has_focus : symbolic_process -> bool

(** Modify the 'has_focus' flag *)
val set_focus : bool -> symbolic_process -> symbolic_process

(** the first term is the channel used for filtering outputs actions, next_function takes the pair (continuation,channel of produced action) *)
val apply_output_filter : Term.term -> (symbolic_process -> unit) -> Recipe.variable -> symbolic_process -> unit

(** return all the choices of focus with the corresponding skeleton of focused process, resulting symbolic process have a focus *)
val list_of_choices_focus : symbolic_process -> (symbolic_process * skeleton) list

(** Given a list of left symbolic processes with a focus and their corresponding skeleton of focused process and a right process,
   it assembles the corresponding answers of the right process (or raises a non-equ exception) *)
val assemble_choices_focus: (symbolic_process * skeleton) list -> symbolic_process -> (symbolic_process * symbolic_process) list

exception Not_eq_left of string
exception Not_eq_right of string
