open Process_
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
	     
let importVar x = Term_.var (Term.display_variable x)

(* We suppose here that processes have been alpha-renamed in order
   to avoid any clash variable-name or name-name. *)
let importName n = Term_.var (Term.display_name n)
    
let rec importPat = function
  | Process.Var x -> importVar x
  | Process.Tuple (s, tl) when Term.is_tuple s -> Term_.tuple (List.map importPat tl)
  | _ -> err "In generalized POR mode, in let p = t in ..., p must be made of tuples and variables only."

let importSymb s t1 = 		(* ugly workaround fo get a more compact function *)
  if Term.is_equal_symbol Term.senc s then 2, Term_.senc t1
  else if Term.is_equal_symbol Term.sdec s then 2, Term_.sdec t1
  else if Term.is_equal_symbol Term.aenc s then 2, Term_.aenc t1
  else if Term.is_equal_symbol Term.adec s then 2, Term_.adec t1
  else if Term.is_equal_symbol Term.hash s then 1, Term_.hash
  else if Term.is_equal_symbol Term.pk s then 1, Term_.pk
  (* TODO: add those constructors in POR. *)
  (* else if Term.is_equal_symbol Term.sk s then Term_.sk *)
  (* else if Term.is_equal_symbol Term.sign s then Term_.sign *)
  (* else if Term.is_equal_symbol Term.checksign s then Term_.checksign *)
  else raise Not_found
	     
let rec importTerm = function
  | Term.Func (symb, tl) when Term.is_tuple symb ->
     Term_.tuple (List.map importTerm tl)
  | Term.Func (symb, []) -> Term_.ok () (* TODO: generic constants != ok *)
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
  | _ -> (Term_.ok (), Term_.ok ()) (* TODO *)
    
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
       match formula with
       | Process.Eq (t1,t2) -> if_eq (importTerm t1) (importTerm t2) (build proc_then) (build proc_else)
       | Process.Neq (t1,t2) -> if_neq (importTerm t1) (importTerm t2) (build proc_then) (build proc_else)
       | _ -> err "In generalized POR mode, tests in conditionals must be equality or disequality (no OR or AND)."
  in
  build proc


module POR = POR.Make(Trace_equiv)
module Persistent = POR.Persistent
module Sleep = POR.Sleep

let runPOR ?(show_sleep=false) s =

  let module S = Trace_equiv in

  let pp_persistent ch s =
    let t0 = Unix.gettimeofday () in
    let set = POR.persistent s in
      Format.fprintf ch "%a (%.2fs)"
        S.ActionSet.pp set (Unix.gettimeofday () -. t0) ;
  in

  (* Some transitions and persistent set computations *)
  Format.printf "s = %a@.@." S.State.pp s ;
  S.fold_successors s ()
    (fun a s' () ->
         Format.printf
           "s -%a-> s'=%a@."
           S.Action.pp a
           S.State.pp s' ;
         Format.printf
           "P(s') = %a@.@."
           pp_persistent s') ;
  Format.printf
    "P(s) = %a@."
    pp_persistent s ;

  (* Number of states and traces in successive transition systems *)

  Format.printf "@." ;

  let module Stats = LTS.Make(Trace_equiv) in
  Format.printf "Equivalence LTS: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;

  let module Stats = LTS.Make(Persistent) in
  Format.printf "Persistent: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;
  let module Stats = LTS.Make(Sleep) in

  let s = Sleep.from_state s in
  Format.printf "Sleep: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;
  if show_sleep then
    Stats.show_traces s ;

  Format.printf "@."

let make_state p1 p2 =
  { Trace_equiv.State.
    left = Sem_utils.Configs.of_process p1 ;
    right = Sem_utils.Configs.of_process p2;
    constraints = Sem_utils.Constraints.empty }

let computeTraces p1 p2 = runPOR (make_state p1 p2)
