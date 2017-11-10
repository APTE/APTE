let importChannel = function
  | Standard_library.Term.Name name -> int_of_char name.id_n
  | _ -> pp "In generalized POR mode, channels should be constants."

let importVar x = Por.Process.var x.id_v

let importName n = Por.Process.var n.id_n
    
let importPat = function
  | Standard_library.Term.Var x -> importVar x
  | _ -> pp "In generalized POR mode, in let p = t in ..., x must be a variable"

let rec importTerm = function
  | Standard_library.Term.Func (f,tl) as x -> x
  | Standard_library.Term.Var x ->  importVar x
  | Standard_library.Term.Name n -> importName n

let importFormula = function
  | _ -> ok ()
    
let importProc proc =
  let rec flatten_choice = function
    | Standard_library.Process.Choice(p1,p2) -> (flatten_choice p1) @ (flatten_choice p2)
    | Standard_library.Process.New(n,p,label) -> flatten_choice p
    | p -> [p]
  and flatten_par = function
    | Standard_library.Process.Par(p1,p2) -> (flatten_par p1) @ (flatten_par p2)
    | Standard_library.Process.New(n,p,label) -> flatten_par p
    | p -> [p]
  and build = function
    | Standard_library.Process.Nil -> zero
    | Standard_library.Process.Choice(p1,p2) -> plus (flatten_choice p1) @ (flatten_choice p2)
    | Standard_library.Process.Par(p1,p2) -> par (flatten_par p1) @ (flatten_par p2)
    | Standard_library.Process.New(n,p,label) -> go_through p
    | Standard_library.Process.In(t,pat,proc,label) -> input (importChan t) (importVar pat) (go_through proc)
    | Standard_library.Process.Out(t1,t2,proc,label) -> output (importChan t1) (importTerm t2) (go_through proc)
    | Standard_library.Process.Let(pat,t,proc,label) -> if_eq (importPat pat) (importTerm t) (go_through proc) zero
    | Standard_library.Process.IfThenElse(formula,proc_then,proc_else,label) ->
       match importFormula formula
       with | (t1,t2) -> if_eq t1 t2 (go_through proc_then) (go_through proc_else)
  in
  build proc
	  
