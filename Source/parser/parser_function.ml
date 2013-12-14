(*************************************************************************
** APTE v0.4beta - Algorithm for Proving Trace Equivalence              **
**                                                                      **
** Copyright (C) 2013  Vincent Cheval                                   **
**                                                                      **
** This program is free software: you can redistribute it and/or modify **
** it under the terms of the GNU General Public License as published by **
** the Free Software Foundation, either version 3 of the License, or    **
** any later version.                                                   **
**                                                                      **
** This program is distributed in the hope that it will be useful,      **
** but WITHOUT ANY WARRANTY; without even the implied warranty of       **
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 **
** See the GNU General Public License for more details.                 **
**                                                                      **
** You should have received a copy of the GNU General Public License    **
** along with this program.  If not, see http://www.gnu.org/licenses/   **
**************************************************************************)

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

type env_elt =
  | Var of Term.variable
  | Name of Term.name
  | Func of Term.symbol
  | Proc of Term.variable list * Process.process
  
type declaration = 
  | ProcDecl of ident * ident list * process
  | FuncDecl of ident * int
  | LengthDecl of ident * (float * float list)
  | LengthTupleDecl of int * (float * float list) * int
  | FreeNameDecl of ident
  | Equivalence of process * process
  | EquivalenceLength of process * process

module StringComp =
struct
  type t = string
  let compare = compare
end

module StringMap = Map.Make(StringComp)

let equivalence_request = ref []

let equivalence_length_request = ref []
	
(***********************************
***         Error_message        ***
************************************)

let display_env_elt_type = function
  | Var _ -> "a variable"
  | Name _ -> "a name"
  | Func _ -> "a function"
  | Proc _ -> "a process"

let error_message line str =
  let error_message = Printf.sprintf "Error! Line %d : %s\n" line str in
  failwith error_message
  
let warning_message line str =
  Printf.printf "Warning! Line %d : %s\n" line str
	
(***********************************
***            Parsing           ***
************************************)  
  
(******** Existing functions *********)

let environment = ref (StringMap.empty:env_elt StringMap.t)

let initialise_environment () = 
  environment := StringMap.empty;
  environment := StringMap.add "senc" (Func Term.senc) !environment;
  environment := StringMap.add "aenc" (Func Term.aenc) !environment;
  environment := StringMap.add "pk" (Func Term.pk) !environment;
  environment := StringMap.add "vk" (Func Term.vk) !environment;
  environment := StringMap.add "sign" (Func Term.sign) !environment;
  environment := StringMap.add "hash" (Func Term.hash) !environment;
  environment := StringMap.add "sdec" (Func Term.sdec) !environment;
  environment := StringMap.add "adec" (Func Term.adec) !environment;
  environment := StringMap.add "checksign" (Func Term.checksign) !environment

(******** Terms ********)

let rec parse_term env = function
  | Id (s,line) ->
      begin try
        match StringMap.find s env with
          | Var(v) -> Term.term_of_variable v
          | Name(n) -> Term.term_of_name n
          | Func(f) when Term.get_arity f = 0 -> Term.apply_function f []
          | env_elt -> error_message line (Printf.sprintf "The identifiant %s is declared as %s but a name or a variable is expected." s (display_env_elt_type env_elt)) 
      with
        Not_found -> error_message line (Printf.sprintf "The identifiant %s is not declared" s)
      end
  | FuncApp((s,line),args) -> 
      begin try
        match StringMap.find s env with
          | Func(f) -> 
              if (Term.get_arity f) <> List.length args
              then error_message line (Printf.sprintf "The function %s is given %d arguments but is expecting %d arguments" s (List.length args) (Term.get_arity f));
              
              Term.apply_function f (List.map (parse_term env) args)
          | env_elt -> error_message line (Printf.sprintf "The identifiant %s is declared as %s but a name or a function is expected." s (display_env_elt_type env_elt)) 
      with
        Not_found -> error_message line (Printf.sprintf "The function %s is not declared" s)
      end
  | Tuple(args) ->
      let f = Term.get_tuple (List.length args) in
      Term.apply_function f (List.map (parse_term env) args)
  | Proj(i,n,t,line) -> 
       if i > n || i < 1 || n < 0
       then error_message line "A projection is necessary of the form \"proj_i_n\" where n > 0, i > 0 and i <= n.";
       let symb_tuple = Term.get_tuple n in
       let symb_proj = Term.nth_projection symb_tuple i in
       Term.apply_function symb_proj [parse_term env t]
 
(******** Pattern ********)      
      
let rec parse_pattern env = function
  | PVar (s,line) ->
      if StringMap.mem s env
      then warning_message line (Printf.sprintf "The identifier %s is already defined." s);
      
      let v = Term.fresh_variable_from_id Term.Free s in
      
      (Process.Var v), StringMap.add s (Var v) env
  | PTuple(args) -> 
      let args',env' = parse_pattern_list env args in
      let f = Term.get_tuple (List.length args) in
      
      Process.Tuple (f,args'), env'
        
and parse_pattern_list env = function 
  | [] -> [], env
  | pat::q -> 
      let pat',env' = parse_pattern env pat in
      let pat_l,env'' = parse_pattern_list env' q in
      pat'::pat_l, env''
      
(******** Formula ********)  
  
let rec parse_formula env = function
  | Eq (t1,t2) -> Process.Eq(parse_term env t1,parse_term env t2)
  | Neq (t1,t2) -> Process.Neq(parse_term env t1, parse_term env t2)
  | And(f1,f2) -> Process.And(parse_formula env f1, parse_formula env f2)
  | Or(f1,f2) -> Process.Or(parse_formula env f1,parse_formula env f2)

(******** Process ********)
  
let rec parse_process env = function
  | Call((s,line),term_list) ->
      begin try
        match StringMap.find s env with
          | Proc(var_list,process) -> 
              if List.length var_list <> List.length term_list
              then error_message line (Printf.sprintf "The process %s is given %d arguments but is expecting %d arguments" s (List.length term_list) (List.length var_list));
              
              let list_t = List.map (parse_term env) term_list in
              let subst = List.fold_left2 (fun subst_acc v t ->
                let subst' = Term.create_substitution v t in
                Term.compose subst_acc subst'
              ) Term.identity var_list list_t in
              
              let process' = Process.rename process in
              Term.apply_substitution subst process' Process.iter_term_process
          | env_elt -> error_message line (Printf.sprintf "The identifiant %s is declared as %s but a process is expected." s (display_env_elt_type env_elt))
      with
        Not_found -> error_message line (Printf.sprintf "The identifiant %s is not declared" s)
      end
  | Nil -> Process.Nil
  | Choice(p1,p2) -> Process.Choice (parse_process env p1, parse_process env p2)
  | Par(p1,p2) -> Process.Par (parse_process env p1, parse_process env p2)
  | New((s,line),proc) ->
      if StringMap.mem s env
      then warning_message line (Printf.sprintf "The identifier %s is already defined." s);
      
      let n = Term.fresh_name_from_id Term.Private s in
      let env' = StringMap.add s (Name n) env in
      
      Process.New(n,parse_process env' proc,Process.fresh_label ())
  | In(ch,(s,line),proc) ->
      if StringMap.mem s env
      then warning_message line (Printf.sprintf "The identifier %s is already defined." s);
      
      let ch' = parse_term env ch in
      let x = Term.fresh_variable_from_id Term.Free s in
      let env' = StringMap.add s (Var x) env in
      
      Process.In(ch',x, parse_process env' proc, Process.fresh_label ())
  | Out(ch,t,proc) ->
      let ch' = parse_term env ch
      and t' = parse_term env t
      and proc' = parse_process env proc in
      
      Process.Out(ch',t',proc', Process.fresh_label ())
  | Let(pat,t,proc) ->
      let t' = parse_term env t in
      let pat',env' = parse_pattern env pat in
      let proc' = parse_process env' proc in
      
      Process.Let(pat',t',proc',Process.fresh_label())
  | IfThenElse(form,proc1,proc2) ->
      let form' = parse_formula env form
      and proc1' = parse_process env proc1
      and proc2' = parse_process env proc2 in
      
      Process.IfThenElse(form',proc1',proc2',Process.fresh_label ())
      
(****** Function declaration *******)

let parse_function_declaration env (s,line) arity = 
  if StringMap.mem s env
  then error_message line (Printf.sprintf "The identifier %s is already defined." s);
  
  let f = Term.new_constructor arity s in
  StringMap.add s (Func f) env
  
(****** Public name declaration *******)

let parse_public_name_declaration env (s,line) = 
  if StringMap.mem s env
  then error_message line (Printf.sprintf "The identifier %s is already defined." s);
  
  let n = Term.fresh_name_from_id Term.Public s in
  StringMap.add s (Name n) env
  
(****** Process declaration ********)

let rec parse_list_argument s_proc added_var env = function
  | [] -> ([],env)
  | (var_s,line)::q ->
      if List.exists (fun var_s' -> var_s = var_s') added_var
      then warning_message line (Printf.sprintf "The variable %s is defined several times as argument of %s" var_s s_proc);
      
      let var = Term.fresh_variable_from_id Term.Free var_s in
      let env' = StringMap.add var_s (Var var) env in
      let (l_var,env'') = parse_list_argument s_proc (var_s::added_var) env' q in
      (var::l_var,env'')

let parse_process_declaration env (s,line) var_list proc = 
  if StringMap.mem s env
  then error_message line (Printf.sprintf "The identifier %s is already defined." s);
    
  let (var_l,env') = parse_list_argument s [] env var_list in
  let proc' = parse_process env' proc in
  
  StringMap.add s (Proc (var_l,proc')) env
  
(******* Length declaration *******)

let parse_length_declaration env (s,line) (cst,args) =
  try
    begin match StringMap.find s env with
      | Func(f) -> 
          if List.mem_assq f !Length_equivalence.Length.length_functions
          then error_message line (Printf.sprintf "The function %s was already given a length function" s);
      
          if (Term.get_arity f) <> List.length args
          then error_message line (Printf.sprintf "The function %s is given %d length coefficient for its arguments but is expecting %d coefficient" s (List.length args) (Term.get_arity f));
              
          Length_equivalence.Length.length_functions := (f,(cst,args)):: !Length_equivalence.Length.length_functions
      | env_elt -> error_message line (Printf.sprintf "The identifiant %s is declared as %s but a function is expected." s (display_env_elt_type env_elt)) 
    
    end
  with Not_found -> error_message line (Printf.sprintf "The identifier %s is not defined." s)
  
(***********************************
***           Main Entry         ***
************************************)

let parse_one_declaration = function
  | ProcDecl (id,var_list,proc) -> 
      environment := parse_process_declaration !environment id var_list proc
  | LengthDecl (id,(cst,args)) ->
      parse_length_declaration !environment id (cst,args)
  | LengthTupleDecl (ar,(cst,args),line) ->  
      let f = Term.get_tuple ar in
      if ar = List.length args
      then Length_equivalence.Length.length_functions := (f,(cst,args)):: !Length_equivalence.Length.length_functions
      else error_message line (Printf.sprintf "The function tuple(%d) is given %d length coefficients for its arguments but is expecting %d coefficients" ar (List.length args) ar);
  | FuncDecl (id,arity) -> 
      environment := parse_function_declaration !environment id arity
  | FreeNameDecl id ->
      environment := parse_public_name_declaration !environment id
  | Equivalence(proc1,proc2) ->
      let p1' = Process.refresh_label (parse_process !environment proc1)
      and p2' = Process.refresh_label (parse_process !environment proc2) in
      
      equivalence_request := (p1',p2')::!equivalence_request
  | EquivalenceLength(proc1,proc2) ->
      let p1' = Process.refresh_label (parse_process !environment proc1)
      and p2' = Process.refresh_label (parse_process !environment proc2) in
      
      equivalence_length_request := (p1',p2')::!equivalence_length_request
