open Porridge.Process			(* Generic POR engine *)
open Standard_library

let tblChannel = Hashtbl.create 5
let intChannel = ref 0
		     
let err s = Debug.internal_error s
let pp s = Printf.printf s
	   
let importChannel = function
  | Term.Name n -> (* Term.Func (s,[]) ->  *)
     let strCh = Term.display_name n in
     let intCh = try Hashtbl.find tblChannel n
		 with Not_found -> begin Hashtbl.add tblChannel n !intChannel;
					 incr(intChannel);
					 !intChannel - 1;
				   end in
     Porridge.Channel.of_int intCh
  | _ -> err "In generalized POR mode, channels must be constants."
	     
let importVar x = Porridge.Term.var (Term.display_variable x)

let importName n = Porridge.Term.var (Term.display_name n)
    
let rec importPat = function
  | Process.Var x -> importVar x
  | Process.Tuple (s, tl) when Term.is_tuple s -> Porridge.Term.tuple (List.map importPat tl)
  | _ -> err "In generalized POR mode, in let p = t in ..., p must be made of tuples and variables only."

let importSymb s t1 = 		(* ugly workaround fo get a more compact function *)
  if Term.is_equal_symbol Term.senc s then 2, Porridge.Term.senc t1
  else if Term.is_equal_symbol Term.sdec s then 2, Porridge.Term.sdec t1
  else if Term.is_equal_symbol Term.aenc s then 2, Porridge.Term.aenc t1
  else if Term.is_equal_symbol Term.adec s then 2, Porridge.Term.adec t1
  else if Term.is_equal_symbol Term.hash s then 1, Porridge.Term.hash
  else if Term.is_equal_symbol Term.pk s then 1, Porridge.Term.pk
  else if Term.is_equal_symbol Term.vk s then 1,Porridge.Term.vk
  else if Term.is_equal_symbol Term.sign s then 2,Porridge.Term.sign t1
  else if Term.is_equal_symbol Term.checksign s then 2,Porridge.Term.checksign t1
  else raise Not_found
	     
let rec importTerm = function
  | Term.Func (symb, tl) when Term.is_tuple symb ->
     Porridge.Term.tuple (List.map importTerm tl)
  | Term.Func (symb, []) -> Porridge.Term.var (Term.display_symbol_without_arity symb) (* constants are abstacted away by variables *)
  | Term.Func (symb, tl) ->
     let t1 = List.hd tl in
     let pt1 = importTerm t1 in
     (try (match importSymb symb pt1 with
	   | 1,f -> f pt1
	   | 2,f ->  let t2 = List.hd (List.tl tl) in
		     let pt2 = importTerm t2 in
		     f pt2)
      with
      | Not_found -> err "In generalized POR mode, allows constructors are senc, sdec, aenc, adec, hash, pk and tuples."
      | _ -> err "[Internal error] Arity does not match.")
  | Term.Var x ->  importVar x
  | Term.Name n -> importName n

let importFormula = function
  | _ -> (Porridge.Term.ok (), Porridge.Term.ok ()) (* TODO *)
    
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
    | Process.New(n,p,label) -> build p (* names will be abstracted away by variables *)
    | Process.In(t,pat,proc,label) -> input (importChannel t) (importVar pat) (build proc)
    | Process.Out(t1,t2,proc,label) -> output (importChannel t1) (importTerm t2) (build proc)
    | Process.Let(pat,t,proc,label) -> if_eq (importPat pat) (importTerm t) (build proc) zero
    | Process.IfThenElse(formula,proc_then,proc_else,label) ->
       match formula with
       | Process.Eq (t1,t2) -> if_eq (importTerm t1) (importTerm t2) (build proc_then) (build proc_else)
       | Process.Neq (t1,t2) -> if_neq (importTerm t1) (importTerm t2) (build proc_then) (build proc_else)
       | _ -> err "In generalized POR mode, tests in conditionals must be equality or disequality (no OR or AND)."
  in
  build proc


(* Cannot pen trace_equiv because then it exposes the interface term.mli -> clash with standard library*)
let computeTraces p1 p2 = POR.tracesPersistentSleepEquiv p1 p2
