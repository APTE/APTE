open Standard_library

(***********************************
***            Types             ***
************************************)

type ident = string * int

type term =
  | Id of ident
  | FuncApp of ident * term list
  | Tuple of term list
  | Proj of int * int * term * int
  
type pattern = 
  | PVar of ident
  | PTuple of pattern list
  
type formula = 
  | Eq of term * term
  | Neq of term * term
  | And of formula * formula
  | Or of formula * formula
    
type process =
  | Call of ident * term list
  | Nil
  | Choice of process * process
  | Par of process * process
  | New of ident * process
  | In of term * ident * process
  | Out of term * term * process
  | Let of pattern * term * process
  | IfThenElse of formula * process * process
  
type declaration = 
  | ProcDecl of ident * ident list * process
  | FuncDecl of ident * int
  | LengthDecl of ident * (float * float list)
  | LengthTupleDecl of int * (float * float list) * int
  | FreeNameDecl of ident
  | Equivalence of process * process
  | EquivalenceLength of process * process
  
(***********************************
***            Parsing           ***
************************************)

val error_message : int -> string -> 'a

val initialise_environment : unit -> unit

val equivalence_request : (Process.process * Process.process) list ref

val equivalence_length_request : (Process.process * Process.process) list ref

val parse_one_declaration : declaration -> unit
