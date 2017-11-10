open Process_
open Standard_library
let pp = Printf.printf
	   
let importChannel ch = Channel.of_int (int_of_string (Term.display_term ch))
(*  | _ -> pp "In generalized POR mode, channels should be constants." TODO *)

let importVar x = Term_.var (Term.display_variable x)

let importName n = Term_.var (Term.display_name n) (* fresh name here ? *)
    
let importPat = function
  | Process.Var x -> importVar x
  | _ -> pp "In generalized POR mode, in let p = t in ..., x must be a variable"; exit 0

let rec importTerm = function
  | Term.Func (symb,tl) ->
     (match symb.name with
      | "enc" when List.length tl = 2 ->
	 let t1, t2 = List.hd tl, List.hd (List.tl tl) in
	 Term_.senc (importTerm t1) (importTerm t2)
      | "dec" when List.length tl = 2 ->
	 let t1, t2 = List.hd tl, List.hd (List.tl tl) in
	 Term_.sdec (importTerm t1) (importTerm t2)
      (* other constructors *)
      | _ when symb.cat = Tuple -> Term_.tuple (List.map importTerm tl)
      | _ when tl = [] -> Term_.ok ()       (* todo: generic constants *)
     )
  | Term.Var x ->  importVar x
  | Term.Name n -> importName n

let importFormula = function
  | _ -> (Term_.ok (), Term_.ok ())
    
let importProcess proc =
  let rec flatten_choice = function
    | Process.Choice(p1,p2) -> (flatten_choice p1) @ (flatten_choice p2)
    | Process.New(n,p,label) -> flatten_choice p
    | p -> [build p]
  and flatten_par = function
    | Process.Par(p1,p2) -> (flatten_par p1) @ (flatten_par p2)
    | Process.New(n,p,label) -> flatten_par p
    | p -> [build p]
  and build = function
    | Process.Nil -> zero
    | Process.Choice(p1,p2) -> plus ((flatten_choice p1) @ (flatten_choice p2))
    | Process.Par(p1,p2) -> par ((flatten_par p1) @ (flatten_par p2))
    | Process.New(n,p,label) -> build p
    | Process.In(t,pat,proc,label) -> input (importChannel t) (importVar pat) (build proc)
    | Process.Out(t1,t2,proc,label) -> output (importChannel t1) (importTerm t2) (build proc)
    | Process.Let(pat,t,proc,label) -> if_eq (importPat pat) (importTerm t) (build proc) zero
    | Process.IfThenElse(formula,proc_then,proc_else,label) ->
       match importFormula formula
       with | (t1,t2) -> if_eq t1 t2 (build proc_then) (build proc_else)
  in
  build proc
	  
