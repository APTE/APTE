(******************
***    Label    ***
*******************)

type label = int

let label_countdown = ref 0

let fresh_label () = 
  let l = !label_countdown in
  label_countdown := l + 1;
  l

type internal_communication = 
  {
    in_label : label;
    out_label : label
  }
  
let rec is_comm_forbidden l_in l_out = function
  | [] -> false
  | f_comm::_ when f_comm.in_label = l_in && f_comm.out_label = l_out -> true
  | _::q -> is_comm_forbidden l_in l_out q
  
let remove_in_label in_label = List.filter (fun f_comm -> f_comm.in_label <> in_label)

let remove_out_label out_label = List.filter (fun f_comm -> f_comm.out_label <> out_label)

let add_forbidden_label in_label out_label list_forbidden_comm = { in_label = in_label; out_label = out_label }::list_forbidden_comm
	
(***************************
***        Formula       ***
****************************)

type formula = 
  | Eq of Term.term * Term.term
  | Neq of Term.term * Term.term
  | And of formula * formula
  | Or of formula * formula
  
let rec negation = function
  | Eq(t1,t2) -> Neq(t1,t2)
  | Neq(t1,t2) -> Eq(t1,t2)
  | And(eq1,eq2) -> Or(negation eq1, negation eq2)
  | Or(eq1,eq2) -> And(negation eq1, negation eq2)
  
let iter_term_formula formula f_apply = 
  let rec go_through = function
    | Eq(t1,t2) -> Eq(f_apply t1,f_apply t2)
    | Neq(t1,t2) -> Neq(f_apply t1,f_apply t2)
    | And(eq1,eq2) -> And(go_through eq1,go_through eq2)
    | Or(eq1,eq2) -> Or(go_through eq1,go_through eq2)
  in
  go_through formula
  
type conjunction_for_csys = 
  | CsysEq of Term.term * Term.term
  | CsysOrNeq of (Term.term * Term.term) list
  
let rec conjunction_from_formula = function
  | Eq(t1,t2) -> [ [CsysEq (t1,t2)] ]
  | Neq(t1,t2) -> [ [CsysOrNeq [t1,t2]] ]
  | And(eq1,eq2) -> 
      let disj_conj_1 = conjunction_from_formula eq1
      and disj_conj_2 = conjunction_from_formula eq2 in
      
      List.fold_left (fun acc conj_1 ->
        (List.map (fun conj_2 -> conj_1 @ conj_2) disj_conj_2)@acc
      ) [] disj_conj_1
  | Or(eq1,eq2) ->
      let disj_conj_1 = conjunction_from_formula eq1
      and disj_conj_2 = conjunction_from_formula eq2 in
      
      let current_neq = ref [] in
      let result = ref [] in
      
      List.iter (function
        |[CsysOrNeq l] -> current_neq := l @ !current_neq 
        |conj -> result := conj :: !result
      ) disj_conj_1;
      
      List.iter (function
        |[CsysOrNeq l] -> current_neq := l @ !current_neq 
        |conj -> result := conj :: !result
      ) disj_conj_2;
      
      [CsysOrNeq(!current_neq)]::!result
      
(**********************************
***           Pattern           ***
***********************************) 

type pattern = 
  | Var of Term.variable
  | Tuple of Term.symbol * pattern list

let rec formula_from_pattern cor_term = function
  | Var v -> [Term.term_of_variable v,cor_term]
  | Tuple(f,args) -> 
      let proj_symb_list = Term.get_projections f in
      
      List.fold_left2 (fun acc pat proj_symb ->
        (formula_from_pattern (Term.apply_function proj_symb [cor_term]) pat)@acc
      ) [] args proj_symb_list
      
(**********************************
***           Process           ***
***********************************) 
  
type process =
  | Nil
  | Choice of process * process
  | Par of process * process
  | New of Term.name * process * label
  | In of Term.term * Term.variable * process * label
  | Out of Term.term * Term.term * process * label
  | Let of pattern * Term.term * process * label
  | IfThenElse of formula * process * process * label
  
let iter_term_process proc f_apply = 
  let rec go_through = function
    | Nil -> Nil
    | Choice(p1,p2) -> Choice(go_through p1,go_through p2)
    | Par(p1,p2) -> Par(go_through p1,go_through p2)
    | New(n,p,label) -> New(n,go_through p,label)
    | In(t,pat,proc,label) -> In(f_apply t,pat,go_through proc,label)
    | Out(t1,t2,proc,label) -> Out(f_apply t1,f_apply t2,go_through proc, label)
    | Let(pat,t,proc,label) -> Let(pat, f_apply t,go_through proc,label)
    | IfThenElse(formula,proc_then,proc_else,label) ->
        IfThenElse(iter_term_formula formula f_apply,go_through proc_then,go_through proc_else,label)
  in
  go_through proc
  
  
(*********************************
***    Public name function    ***
**********************************)
	
let rec concact_exclusive_list list_name_1 list_name_2 = match list_name_1 with
  | [] -> list_name_2
  | t::q when List.exists (Term.is_equal_name t) list_name_2 -> concact_exclusive_list q list_name_2
  | t::q -> t::(concact_exclusive_list q list_name_2)
  
let rec public_name_term t = 
  if Term.is_name_status Term.Public t
  then [Term.name_of_term t]
  else if Term.is_function t
  then Term.fold_left_args (fun l t' -> concact_exclusive_list (public_name_term t') l) [] t
  else []
	
let rec public_name_equations = function
  | Eq(t1,t2) -> concact_exclusive_list (public_name_term t1) (public_name_term t2)
  | Neq(t1,t2) -> concact_exclusive_list (public_name_term t1) (public_name_term t2)
  | And(eq_1,eq_2) -> concact_exclusive_list (public_name_equations eq_1) (public_name_equations eq_2)	
  | Or(eq_1,eq_2) -> concact_exclusive_list (public_name_equations eq_1) (public_name_equations eq_2)
  	
let rec get_free_names process = match process with
  | Nil -> []
  | Par(p1,p2) -> concact_exclusive_list (get_free_names p1) (get_free_names p2)
  | Choice(p1,p2) -> concact_exclusive_list (get_free_names p1) (get_free_names p2)
  | New(_ ,p,_) -> get_free_names p
  | In(t,_,proc,_) -> concact_exclusive_list (public_name_term t) (get_free_names proc)
  | Out(t1,t2,p,_) -> 
      concact_exclusive_list (public_name_term t1) (
        concact_exclusive_list (public_name_term t2) (
          get_free_names p
        )
      )
  | Let(_,t,proc,_) -> concact_exclusive_list (public_name_term t) (get_free_names proc)
  | IfThenElse(eq,proc_then,proc_else,_) -> 
      concact_exclusive_list (public_name_equations eq) (
        concact_exclusive_list (get_free_names proc_then) (
          get_free_names proc_else
        )
      )	

(*********************************
***      Process renaming      ***
**********************************)

let variable_assoc = ref []

let name_assoc = ref []

let rename_term term = Term.rename !variable_assoc !name_assoc term
  
let rec rename_formula = function
  | Eq(t1,t2) -> Eq(rename_term t1,rename_term t2)
  | Neq(t1,t2) -> Neq(rename_term t1, rename_term t2)
  | And(f1,f2) -> And(rename_formula f1,rename_formula f2)
  | Or(f1,f2) -> Or(rename_formula f1,rename_formula f2)
  
let rec rename_pattern = function
  | Var(v) -> 
      let v' = Term.fresh_variable_from_var v in
      variable_assoc := (v,v')::!variable_assoc;
      Var(v')
  | Tuple(f,pat_l) -> Tuple(f,List.map rename_pattern pat_l)
  
let rec rename_process = function
  | Nil -> Nil
  | Choice(p1,p2) -> Choice(rename_process p1,rename_process p2)
  | Par(p1,p2) -> Par(rename_process p1,rename_process p2)
  | New(n,p,l) -> 
      let n' = Term.fresh_name_from_name n in
      name_assoc := (n,n')::!name_assoc;
      New(n',rename_process p,l)
  | In(ch,x,proc,l) ->
      let x' = Term.fresh_variable_from_var x in
      variable_assoc := (x,x')::!variable_assoc;
      In(rename_term ch, x', rename_process proc,l)
  | Out(ch,t,proc,l) -> Out(rename_term ch, rename_term t, rename_process proc,l)
  | Let(pat,t,proc,l) ->
      let pat' = rename_pattern pat in
      let t' = rename_term t
      and proc' = rename_process proc in
      Let(pat',t',proc',l)
  | IfThenElse(eq,proc1,proc2,l) ->
      let eq' = rename_formula eq in
      let proc1' = rename_process proc1 in
      let proc2' = rename_process proc2 in
      IfThenElse(eq',proc1',proc2',l)
      
let rename proc = 
  if !variable_assoc <> [] || !name_assoc <> []
  then Debug.internal_error "[Process.ml >> rename] The association lists should be empty";
  
  let proc' = rename_process proc in
  
  variable_assoc := [];
  name_assoc := [];
  proc'
  
      
(*********************************
***       Label refresh        ***
**********************************)
      
let rec refresh_label_process = function
  | Nil -> Nil
  | Choice(p1,p2) -> 
      let p1' = refresh_label_process p1 in
      let p2' = refresh_label_process p2 in
      Choice(p1',p2')
  | Par(p1,p2) -> 
      let p1' = refresh_label_process p1 in
      let p2' = refresh_label_process p2 in
      Par(p1',p2')
  | New(n,proc,_) ->
      let label = fresh_label () in
      let proc' = refresh_label_process proc in
      New(n,proc',label)
  | In(ch,x,proc,_) -> 
      let label = fresh_label () in
      let proc' = refresh_label_process proc in
      In(ch,x,proc',label)
  | Out(ch,t,proc,_) ->
      let label = fresh_label () in
      let proc' = refresh_label_process proc in
      Out(ch,t,proc',label)
  | Let(pat,t,proc,_) ->
      let label = fresh_label () in
      let proc' = refresh_label_process proc in
      Let(pat,t,proc',label)
  | IfThenElse(eq,proc1,proc2,_) ->
      let label = fresh_label () in
      let proc1' = refresh_label_process proc1 in
      let proc2' = refresh_label_process proc2 in
      IfThenElse(eq,proc1',proc2',label)
      
let refresh_label proc = 
  let old_label = !label_countdown in
  label_countdown := 1;
  
  let result = refresh_label_process proc in
  label_countdown := old_label;
  result
      
(*************************************
	   Symbolic process
**************************************)

(* par_label denotes the position of a sub-process in the 'tree of parallel compositions' *)
type par_label = int list
(* flag_label describes the state of the labelling process (to be labelled or have been labelled (OK)) *)
type flag_label = ToLabel | OkLabel | Dummy
(* a proc_label will be associated to any process in the multiset *)
type proc_label = (par_label * flag_label)

let dummy_l = ([], Dummy)
let init_par_label = [0]
let init_proc_label =(init_par_label, OkLabel) 

let set_to_be_labelled = function
  | (parLab,_) -> (parLab, ToLabel)

(* TODO: par_label are not used yet -> to remove if not needed *)
type trace_label =
  | Output of label * Recipe.recipe * Term.term * Recipe.axiom * Term.term * par_label
  | Input of label * Recipe.recipe * Term.term * Recipe.recipe * Term.term * par_label
  | Comm of internal_communication  (* won't be produced when with_por = true*)
  
type blocks = {
  par_lab : par_label;
  inp : Recipe.recipe list;
  out : Recipe.axiom list;
  mutable complete_inp : bool;	(* is set to true when performing a release of focus *)
(* TODO: is mutable dangerous here? when backtracking???????? CHECK THIS OUT *)
}

type symbolic_process = 
  {
    axiom_name_assoc : (Recipe.recipe * Term.term) list;
    process : (process*proc_label) list; (* Represent a multiset of labelled processes *)
    has_focus : bool;	    (* if true: process[0] is under focus, otherwise: no focus *)
    is_improper : bool;	    (* if true, last block is improper -> cannot be executed any more *)
    constraint_system : Constraint_system.constraint_system;
    forbidden_comm : internal_communication list;
    trace : trace_label list;
    trace_blocks : blocks list;
    marked : bool
  }

let pp s = Printf.printf "%s%!" s

let block_set_complete_inp symP = 
  let trace_blocks = symP.trace_blocks in
  if not(trace_blocks = [])
  then (List.hd trace_blocks).complete_inp <- true
					       
let block_complete_inp symP =
  let trace_blocks = symP.trace_blocks in
  trace_blocks <> [] && (List.hd symP.trace_blocks).complete_inp

let create_symbolic axiom_name_assoc proc csys = 
  {
    axiom_name_assoc = axiom_name_assoc;
    process = [(proc, init_proc_label)];
    has_focus = false;
    is_improper = false;
    constraint_system = csys;
    forbidden_comm = [];
    trace = [];
    trace_blocks = [];
    marked = false
  }
  
(******* Display *******)

let ps = Printf.sprintf

(* Warning: I use list.rev here to pretty print par_labs. par_labs should be small
   but it still can slow down the program. *)
let display_parlab pl = 
  let pl_rev = List.tl (List.rev pl) in (* we do not display the first '0' *)
  (List.fold_left
     (fun str_acc i -> (str_acc^(string_of_int i)))
     "[" pl_rev)^"]"
	       
let display_block b =
  if b.complete_inp
  then ps "{%s:%s/%s}" 
	  (display_parlab b.par_lab)
	  ((List.fold_left
	      ((fun str_acc r -> (str_acc^(Recipe.display_recipe r))^","))
	      "" b.inp)^"")
	  ((List.fold_left
	      ((fun str_acc a -> (str_acc^(Recipe.display_axiom a))^","))
	      "" b.out)^"")
  else ps "{{%s:%s/.}}" 
	  (display_parlab b.par_lab)
	  ((List.fold_left
	      ((fun str_acc s -> (str_acc^(Recipe.display_recipe s))^","))
	      "" b.inp)^"")

let display_trace_blocks symP = 
  ps "Trace_Blocks:  [%s].\n"
     (List.fold_left
	(fun str_acc b -> (str_acc^(display_block b))^" ")
	"" symP.trace_blocks)
     
let display_trace_label r_subst m_subst recipe_term_assoc = function 
  | Output(label,r_ch,m_ch,ax,t,pl) -> 
      let r_ch' = Recipe.apply_substitution r_subst r_ch (fun t f -> f t) in
      let m_ch' = Term.apply_substitution m_subst m_ch (fun t f -> f t) in
      let t' = Term.apply_substitution m_subst t (fun t f -> f t) in
        
      Printf.sprintf "Output {%d} (parlab: %s) on the channel %s (obtain by %s) of the term %s (axiom %s)\n" 
        label 
	(display_parlab pl)
        (Term.display_term m_ch')
        (Recipe.display_recipe2 recipe_term_assoc Term.display_term r_ch')
        (Term.display_term t')
        (Recipe.display_axiom ax)
  | Input(label,r_ch,m_ch,r_t,m_t,pl) ->
      let r_ch' = Recipe.apply_substitution r_subst r_ch (fun t f -> f t) in
      let m_ch' = Term.apply_substitution m_subst m_ch (fun t f -> f t) in
      let r_t' = Recipe.apply_substitution r_subst r_t (fun t f -> f t) in
      let m_t' = Term.apply_substitution m_subst m_t (fun t f -> f t) in
        
      Printf.sprintf "Input {%d} (parlab: %s) on the channel %s (obtain by %s) of the term %s (obtain by %s)\n" 
        label 
	(display_parlab pl)
        (Term.display_term m_ch')
        (Recipe.display_recipe2 recipe_term_assoc Term.display_term r_ch')
        (Term.display_term m_t')
        (Recipe.display_recipe2 recipe_term_assoc Term.display_term r_t')
  | Comm(intern) ->
      Printf.sprintf "Internal communication between the input {%d} and output {%d}\n" intern.in_label intern.out_label
  

let display_trace symb_proc = 
  let message_eq = ref (Constraint_system.get_message_equations symb_proc.constraint_system)
  and recipe_eq = Constraint_system.get_recipe_equations symb_proc.constraint_system in
  
  let recipe_term_assoc = ref symb_proc.axiom_name_assoc in
  
  let public_names_intruder = ref [] in
  
  Constraint.iter Constraint.SAll (fun dc ->
    let v = Constraint.Deducibility.get_recipe_variable dc
    and t = Constraint.Deducibility.get_message dc in
    
    let n = Term.fresh_name_from_id Term.Public "n" in
    recipe_term_assoc := (Recipe.recipe_of_variable v, Term.term_of_name n)::!recipe_term_assoc;
    message_eq := (t,Term.term_of_name n)::!message_eq;
    public_names_intruder := n::!public_names_intruder
  ) (Constraint_system.get_deducibility_constraint_set symb_proc.constraint_system);
  
  let m_subst = Term.unify !message_eq
  and r_subst = Recipe.unify recipe_eq in
  
  let rec display_list_names = function
    |[] -> ""
    |[n] -> Term.display_name n
    |n::q -> (Term.display_name n)^", "^(display_list_names q)
  in
  
  let intro = 
    if !public_names_intruder <> []
    then Printf.sprintf "The intruder starts by creating %d fresh names: %s\n" (List.length !public_names_intruder) (display_list_names !public_names_intruder)
    else ""
  in
  
  List.fold_left (fun str_acc tr_label ->
    str_acc^(display_trace_label r_subst m_subst !recipe_term_assoc tr_label)
  ) intro (List.rev symb_proc.trace)

let display_trace_simple symb_proc = 
  let trace = symb_proc.trace in
  let rec display_trace_label_simple = function
    | Output (lab, _, _, _, _, pl) -> Printf.sprintf "Out{%d}%s" lab (display_parlab pl)
    | Input (lab, _, _, _, _, pl) -> Printf.sprintf "In{%d}%s" lab (display_parlab pl)
    | Comm _ -> "SHOULD NOT HAPPEN\n\nBug: see process.ml > display_traces_simple !!\n\n" in
  "[| " ^ (List.fold_left (fun str_acc tr_label ->
			 str_acc^(display_trace_label_simple tr_label)^"; "
			) "" (List.rev trace))^" |]"
	  
(******* Testing ********)

let is_bottom symb_proc = Constraint_system.is_bottom symb_proc.constraint_system
  
(******* Access and modification ********)

let get_constraint_system symb_proc = symb_proc.constraint_system

let replace_constraint_system csys symb_proc = { symb_proc with constraint_system = csys }

let simplify symb_proc = { symb_proc with constraint_system = Constraint_system.Phase_1.normalise symb_proc.constraint_system }
  
let size_trace symb_proc = List.length symb_proc.trace

let map_subst_term trace_l f_apply = 
  List.map (function 
    | Output(label,r_ch,m_ch,ax,t,pl) -> Output(label,r_ch,f_apply m_ch, ax, f_apply t,pl)
    | Input(label,r_ch,m_ch,r_t,m_t,pl) -> Input(label,r_ch,f_apply m_ch, r_t, f_apply m_t,pl)
    | Comm(intern) -> Comm(intern)
  ) trace_l
  
let map_subst_recipe trace_l f_apply = 
  List.map (function 
    | Output(label,r_ch,m_ch,ax,t,pl) -> Output(label,f_apply r_ch,m_ch, ax, t, pl)
    | Input(label,r_ch,m_ch,r_t,m_t,pl) -> Input(label,f_apply r_ch,m_ch, f_apply r_t, m_t, pl)
    | Comm(intern) -> Comm(intern)
  ) trace_l

let instanciate_trace symb_proc = 
  let message_eq = Constraint_system.get_message_equations symb_proc.constraint_system in
  let recipe_eq = Constraint_system.get_recipe_equations symb_proc.constraint_system in
  
  let subst_r = Recipe.unify recipe_eq in

  let symb_proc_1 = {symb_proc with trace = Recipe.apply_substitution subst_r symb_proc.trace map_subst_recipe} in
  {symb_proc_1 with trace = Term.unify_and_apply message_eq symb_proc_1.trace map_subst_term}

(******* Transition application ********)  
  
let apply_internal_transition_without_comm with_por function_next symb_proc = 
  let has_broken_focus = ref false in
  let has_broken_focus_0_case = ref false in
  let was_with_focus = symb_proc.has_focus in

  let rec go_through prev_proc csys = function
    (* when we have gone trough all processes (no more conditionals at top level) *)
    | [] -> 
       let is_improper = symb_proc.is_improper ||
			   (symb_proc.has_focus && (!has_broken_focus_0_case)) in
       function_next { symb_proc with process = prev_proc;
					   constraint_system = csys;
					   is_improper = is_improper;
					   has_focus = symb_proc.has_focus && not(!has_broken_focus);
			  }
    (* Nil case: it enables to release the focus BUT produces an improper block -> set the two flags to true *)
    | (Nil,_)::q -> has_broken_focus := true;
		    has_broken_focus_0_case := true;
		    go_through prev_proc csys q 
    | (Choice(p1,p2), l)::q -> 
       if with_por
       then Debug.internal_error "[process.ml >> apply_internal_transition_without_comm] Inputted processes are not action-deterministic (they use a Choice)."
       else begin
           go_through prev_proc csys ((p1,dummy_l)::q);
           go_through prev_proc csys ((p2,dummy_l)::q);
	 end
    | (Par(p1,p2), l)::q ->
       let l' = if with_por
		then set_to_be_labelled l (* we add a flag meaning that produced sub_processes must be relablled
					     since we broke a parallel composition *)
		else dummy_l in
       has_broken_focus := true;
       go_through prev_proc csys ((p1,l')::(p2,l')::q)
    | (New(_,p,_), l)::q -> go_through prev_proc csys ((p,l)::q)
    | (Let(pat,t,proc,_), l)::q ->
        let eq_to_unify = formula_from_pattern t pat in
        let proc' = Term.unify_and_apply eq_to_unify proc iter_term_process in
        go_through prev_proc csys ((proc',l)::q)      
    | (IfThenElse(formula,proc_then,proc_else,_), l)::q ->
        let disj_conj_then = conjunction_from_formula formula
        and disj_conj_else = conjunction_from_formula (negation formula) in
	(* for any way to satisfy formula or not(formula), branch and (recursive) call go_through *)
        List.iter (fun conj_then ->
          let new_csys = 
            List.fold_left (fun csys_acc -> function
              | CsysEq(t1,t2) -> Constraint_system.add_message_equation csys_acc t1 t2 
              | CsysOrNeq (l) -> 
                  let formula' = Term.create_disjunction_inequation l in
                  Constraint_system.add_message_formula csys_acc formula' 
            ) csys conj_then
          in
          go_through prev_proc new_csys ((proc_then,l)::q)            
        ) disj_conj_then;
        
        List.iter (fun conj_else ->
		   let new_csys = 
		     List.fold_left (fun csys_acc -> function
						  | CsysEq(t1,t2) -> Constraint_system.add_message_equation csys_acc t1 t2 
						  | CsysOrNeq (l) -> 
						     let formula = Term.create_disjunction_inequation l in
						     Constraint_system.add_message_formula csys_acc formula 
				    ) csys conj_else
		   in
		   go_through prev_proc new_csys ((proc_else,l)::q)            
		  ) disj_conj_else
    (* otherwise, proc starts with an input our output -> add it to prev_proc and keep scanning
     the rest of processes *)
    | proc::q -> if with_por && was_with_focus && not(!has_broken_focus)
		 then function_next { symb_proc with process = proc :: q; (* in that case (has_broken_focus = false), we know that prev = [] *)
						     constraint_system = csys;
						     has_focus = symb_proc.has_focus && not(!has_broken_focus)
				    }
		 else go_through (proc::prev_proc) csys q in
  
  go_through [] symb_proc.constraint_system symb_proc.process
	     
(* We assume in this function that the internal transition except the communication have been applied.
 It is not executed if with_comm = true *)  
let rec apply_one_internal_transition_with_comm function_next symb_proc = 

  let rec go_through prev_proc_1 forbid_comm_1 = function
    | [] -> function_next { symb_proc with forbidden_comm = forbid_comm_1 }
    | ((In(ch_in,v,sub_proc_in,label_in),_) as proc_in)::q_1 -> 
        let rec search_for_a_out prev_proc_2 forbid_comm_2 = function
          | [] -> go_through (proc_in::prev_proc_1) forbid_comm_2 q_1
          | ((Out(ch_out,t_out, sub_proc_out, label_out),_) as proc_out)::q_2 ->
              if is_comm_forbidden label_in label_out forbid_comm_2
              then search_for_a_out (proc_out::prev_proc_2) forbid_comm_2 q_2
              else 
                begin
                  (* Case where the internal communication happen *)
                  let new_csys_comm_1 = Constraint_system.add_message_equation symb_proc.constraint_system (Term.term_of_variable v) t_out  in
                  let new_csys_comm_2 = Constraint_system.add_message_equation new_csys_comm_1 ch_in ch_out  in
                  
                  let new_forbid_comm_1 = remove_in_label label_in forbid_comm_2 in
                  let new_forbid_comm_2 = remove_out_label label_out new_forbid_comm_1 in
                  
                  let symb_proc_1 = 
                    { symb_proc with
                      process = prev_proc_1@((sub_proc_in,dummy_l)::(sub_proc_out,dummy_l)::prev_proc_2)@q_2;
                      constraint_system = new_csys_comm_2; 
                      forbidden_comm = new_forbid_comm_2;
                      trace = (Comm { in_label = label_in; out_label = label_out })::symb_proc.trace;
                    }
                  in
                  
                  apply_internal_transition_without_comm false (* with_por must be false since with_comm = true *)
                    (apply_one_internal_transition_with_comm function_next) symb_proc_1;
                    
                  (* Case where the internal communication did not happen *)
                    
                  let forbid_comm_3 = add_forbidden_label label_in label_out forbid_comm_2 in
                  
                  search_for_a_out (proc_out::prev_proc_2) forbid_comm_3 q_2
                end
          | proc::q_2 -> search_for_a_out (proc::prev_proc_2) forbid_comm_2 q_2
        in
        search_for_a_out [] forbid_comm_1 q_1
        
    | ((Out(ch_out,t_out,sub_proc_out,label_out),_) as proc_out)::q_1 -> 
        let rec search_for_a_in prev_proc_2 forbid_comm_2 = function
          | [] -> go_through (proc_out::prev_proc_1) forbid_comm_2 q_1
          | ((In(ch_in,v, sub_proc_in, label_in),_) as proc_in)::q_2 ->
              if is_comm_forbidden label_in label_out forbid_comm_2
              then search_for_a_in (proc_in::prev_proc_2) forbid_comm_2 q_2
              else 
                begin
                  (* Case where the internal communication happen *)
                  let new_csys_comm_1 = Constraint_system.add_message_equation symb_proc.constraint_system (Term.term_of_variable v) t_out  in
                  let new_csys_comm_2 = Constraint_system.add_message_equation new_csys_comm_1 ch_in ch_out  in
                  
                  let new_forbid_comm_1 = remove_in_label label_in forbid_comm_2 in
                  let new_forbid_comm_2 = remove_out_label label_out new_forbid_comm_1 in
                  
                  let symb_proc_1 = 
                    { symb_proc with
                      process = prev_proc_1@((sub_proc_in,dummy_l)::(sub_proc_out,dummy_l)::prev_proc_2)@q_2;
                      constraint_system = new_csys_comm_2; 
                      forbidden_comm = new_forbid_comm_2;
                      trace = (Comm { in_label = label_in; out_label = label_out })::symb_proc.trace
                    }
                  in
                  
                  apply_internal_transition_without_comm false (* with_por must be false since with_comm = true *)
                    (apply_one_internal_transition_with_comm function_next) symb_proc_1;
                    
                  (* Case where the internal communication did not happen *)
                    
                  let forbid_comm_3 = add_forbidden_label label_in label_out forbid_comm_2 in
                  
                  search_for_a_in (proc_in::prev_proc_2) forbid_comm_3 q_2
                end
          | proc::q_2 -> search_for_a_in (proc::prev_proc_2) forbid_comm_2 q_2
        in
        search_for_a_in [] forbid_comm_1 q_1
    | _::_ -> Debug.internal_error "[process.ml >> apply_one_internal_transition_with_comm] The processes in the multiset should all start with either an input or output at this point"

  in
  go_through [] symb_proc.forbidden_comm symb_proc.process
  
let apply_internal_transition with_comm with_por function_next symb_proc = 
  if with_comm
  then 
    if with_por
    then Debug.internal_error "[process.ml >> appply_one_internal_transition] The flags with_comm and with_por cannot be both true."
    else  apply_internal_transition_without_comm
	    with_por
	    (apply_one_internal_transition_with_comm function_next)
	    symb_proc
  else apply_internal_transition_without_comm with_por function_next symb_proc
  
(*************************************
	   External Transition
**************************************)  
 
(* We assume in this function that all internal transitions have been applied *)  
let apply_input with_por function_next ch_var_r t_var_r symb_proc = 
  
  let rec go_through prev_proc = function
    | [] -> ()
    | ((In(ch,v,sub_proc,label),l) as proc)::q ->
        let y = Term.fresh_variable_from_id Term.Free "y" in
        let t_y = Term.term_of_variable y in
        
        let new_csys_1 = Constraint_system.add_new_deducibility_constraint symb_proc.constraint_system ch_var_r t_y  in
        let new_csys_2 = Constraint_system.add_new_deducibility_constraint new_csys_1  t_var_r (Term.term_of_variable v) in
        let new_csys_3 = Constraint_system.add_message_equation new_csys_2 t_y ch in
        
        let ch_r = Recipe.recipe_of_variable ch_var_r
        and t_r = Recipe.recipe_of_variable t_var_r in

	let old_trace_blocks = symb_proc.trace_blocks in
	let new_trace_blocks = 
          if old_trace_blocks = [] or (List.hd old_trace_blocks).complete_inp
	  (* then: we start a new block *)
	  then ({par_lab = fst l;
		 inp = [t_r];
		 out = [];
		 complete_inp = false;
	       }) :: old_trace_blocks
	  (* else: we complete the block with the new recipe *)
	  else begin
	      let block = List.hd old_trace_blocks in
	      let old_traces = List.tl old_trace_blocks in
	      let new_block = {
		block with
		inp =(t_r) :: (block.inp);
	      } in
	      new_block :: old_traces
	    end in
	
        let symb_proc' = 
          { symb_proc with
            process = ((sub_proc,l)::q)@prev_proc;
            constraint_system = new_csys_3;
            forbidden_comm = remove_in_label label symb_proc.forbidden_comm;
            trace = (Input (label,ch_r,Term.term_of_variable y,t_r,Term.term_of_variable v, fst l))::symb_proc.trace;
	    trace_blocks = new_trace_blocks;
          }
        in
        
        function_next (symb_proc',ch);
        if not(with_por)	(* if with_por=true we only consider performing the process under focus *)
	then go_through (proc::prev_proc) q
    | proc::q -> if not(with_por)	(* if with_por=true we only consider performing the process under focus *)
		 then go_through (proc::prev_proc) q
  in
  
  go_through [] symb_proc.process
  
let apply_output with_por function_next ch_var_r symb_proc = 
  
  let rec go_through prev_proc = function
    | [] -> ()
    | ((Out(ch,t,sub_proc,label),l) as proc)::q ->
        let y = Term.fresh_variable_from_id Term.Free "y"
        and x = Term.fresh_variable_from_id Term.Free "x" in
        
        let t_y = Term.term_of_variable y
        and t_x = Term.term_of_variable x in
        
        let new_csys_1 = Constraint_system.add_new_deducibility_constraint symb_proc.constraint_system ch_var_r t_y  in
        let new_csys_2 = Constraint_system.add_message_equation new_csys_1 ch  (Term.term_of_variable y) in
        let new_csys_3 = Constraint_system.add_new_axiom new_csys_2 t_x in
        let new_csys_4 = Constraint_system.add_message_equation new_csys_3 t_x t in
        
        let ch_r = Recipe.recipe_of_variable ch_var_r in
        
	let axiom = Recipe.axiom (Constraint_system.get_maximal_support new_csys_4) in

	(* Update of the last block *)
	let old_trace_blocks = symb_proc.trace_blocks in
	let old_block = List.hd old_trace_blocks in
	let old_blocks = List.tl old_trace_blocks in
	let new_block = {
	  old_block with
	  out = axiom :: (old_block.out);
	} in


        let symb_proc' = 
          { symb_proc with
            process = ((sub_proc,l)::q)@prev_proc;
            constraint_system = new_csys_4;
            forbidden_comm = remove_out_label label symb_proc.forbidden_comm;
            trace = (Output (label,ch_r,Term.term_of_variable y,
			     axiom,
			     Term.term_of_variable x, fst l))::symb_proc.trace;
	    trace_blocks = new_block :: old_blocks;
          }
        in
        
        function_next (symb_proc',ch);
        if not(with_por)
	then go_through (proc::prev_proc) q
    | proc::q -> go_through (proc::prev_proc) q
  in
  
  go_through [] symb_proc.process

let apply_output_filter ch_f function_next ch_var_r symb_proc = 
  
  let rec go_through prev_proc = function
    | [] -> ()
    | ((Out(ch,t,sub_proc,label),l) as proc)::q when Term.is_equal_term ch_f ch ->
       let y = Term.fresh_variable_from_id Term.Free "y"
       and x = Term.fresh_variable_from_id Term.Free "x" in
       
       let t_y = Term.term_of_variable y
       and t_x = Term.term_of_variable x in
       
       let new_csys_1 = Constraint_system.add_new_deducibility_constraint symb_proc.constraint_system ch_var_r t_y  in
       let new_csys_2 = Constraint_system.add_message_equation new_csys_1 ch  (Term.term_of_variable y) in
       let new_csys_3 = Constraint_system.add_new_axiom new_csys_2 t_x in
       let new_csys_4 = Constraint_system.add_message_equation new_csys_3 t_x t in
       
       let ch_r = Recipe.recipe_of_variable ch_var_r in
       
       let axiom = Recipe.axiom (Constraint_system.get_maximal_support new_csys_4) in
       
       (* Update of the last block *)
       let old_trace_blocks = symb_proc.trace_blocks in
       let old_block = List.hd old_trace_blocks in
       let old_blocks = List.tl old_trace_blocks in
       let new_block = {
	 old_block with
	 out = axiom :: (old_block.out);
       } in
       
       let symb_proc' = 
         { symb_proc with
           process = ((sub_proc,l)::q)@prev_proc;
           constraint_system = new_csys_4;
           forbidden_comm = remove_out_label label symb_proc.forbidden_comm;
           trace = (Output (label,ch_r,Term.term_of_variable y,
			    axiom,
			    Term.term_of_variable x, fst l))::symb_proc.trace;
	   trace_blocks = new_block :: old_blocks;
								
         }
       in
       function_next symb_proc'
    | proc::q -> go_through (proc::prev_proc) q
  in
  
  go_through [] symb_proc.process


(*************************************
	   Display function
**************************************)  

let rec display_formula and_prev or_prev = function
  | Neq(t1,t2) -> Printf.sprintf "%s <> %s" (Term.display_term t1) (Term.display_term t2)
  | Eq(t1,t2) -> Printf.sprintf "%s = %s" (Term.display_term t1) (Term.display_term t2)
  | And(eq1,eq2) -> 
      if or_prev
      then Printf.sprintf "(%s && %s)" (display_formula true false eq1) (display_formula true false eq2)
      else Printf.sprintf "%s && %s" (display_formula true false eq1) (display_formula true false eq2)
  | Or(eq1,eq2) ->
      if and_prev
      then Printf.sprintf "(%s || %s)" (display_formula false true eq1) (display_formula false true eq2)
      else Printf.sprintf "%s || %s" (display_formula false true eq1) (display_formula false true eq2)
           
let rec display_list_pattern = function
  | [] -> ""
  | [t] -> display_pattern t
  | t::q -> Printf.sprintf "%s,%s" (display_pattern t) (display_list_pattern q)      
      
and display_pattern = function
  | Var(v) -> Term.display_variable v
  | Tuple(_,args) -> Printf.sprintf "(%s)" (display_list_pattern args)
      
let rec create_depth_tabulation = function
  |0 -> ""
  |depth -> "  "^(create_depth_tabulation (depth-1))
      
let rec sub_display_process n_tab prev_choice prev_par prev_in_out = function
  | Nil -> 
       if prev_in_out
       then ""
       else 
         let tab = create_depth_tabulation (n_tab-1) in
         tab^"0\n"
  | Choice(p1,p2) when prev_choice ->
      let tab = create_depth_tabulation (n_tab-1) in
      let line = tab^")+(\n" in
      
      (sub_display_process n_tab true false false p1)^line^(sub_display_process n_tab true false false p2)
  | Choice(p1,p2) ->
      let tab = create_depth_tabulation n_tab in
      
      let line1 = tab^"(\n"
      and line2 = tab^") + (\n"
      and line3 = tab^")\n" in
      
      line1^(sub_display_process (n_tab+1) true false false p1)^line2^(sub_display_process (n_tab+1) true false false p2)^line3
  | Par(p1,p2) when prev_par ->
      let tab = create_depth_tabulation (n_tab-1) in
      let line = tab^") | (\n" in
      
      (sub_display_process n_tab false true false p1)^line^(sub_display_process n_tab false true false p2)
  | Par(p1,p2) ->
      let tab = create_depth_tabulation n_tab in
      
      let line1 = tab^"(\n"
      and line2 = tab^") | (\n"
      and line3 = tab^")\n" in
      
      line1^(sub_display_process (n_tab+1) false true false p1)^line2^(sub_display_process (n_tab+1) false true false p2)^line3
  | New(n,p,label) ->
      let tab = create_depth_tabulation n_tab in
      Printf.sprintf "%s{%d} new %s.\n%s" tab label (Term.display_name n) (sub_display_process n_tab false false false p)
  | In(ch,v,p,label) ->
      let tab = create_depth_tabulation n_tab in
      
      Printf.sprintf "%s{%d} in(%s,%s);\n%s" tab label
        (Term.display_term ch)
        (Term.display_variable v)
        (sub_display_process n_tab false false true p)
  | Out(ch,t,p,label) ->
      let tab = create_depth_tabulation n_tab in
      
      Printf.sprintf "%s{%d} out(%s,%s);\n%s" tab label
        (Term.display_term ch)
        (Term.display_term t)
        (sub_display_process n_tab false false true p)
  | Let(pat,t,p,label) ->
      let tab = create_depth_tabulation n_tab in
  
      Printf.sprintf "%s{%d} let %s = %s in\n%s" tab label
        (display_pattern pat)
        (Term.display_term t)
        (sub_display_process n_tab false false false p)
  | IfThenElse(form,p1,Nil,label) ->
      let tab = create_depth_tabulation n_tab in
  
      let line = Printf.sprintf "%s{%d} if %s then\n" tab label (display_formula false false form) in

      line^(sub_display_process (n_tab+1) false false false p1)
  | IfThenElse(form,p1,p2,label) ->
      let tab = create_depth_tabulation n_tab in
  
      let line = Printf.sprintf "%s{%d} if %s then\n" tab label (display_formula false false form) in

      line^(sub_display_process (n_tab+1) false false false p1)^tab^"else\n"^(sub_display_process (n_tab+1) false false false p2)
      
let display_process = sub_display_process 0 false false false

let display_trace_label_no_unif m_subst recipe_term_assoc = function 
  | Output(label,r_ch,m_ch,ax,t,pl) -> 
      Printf.sprintf "Output {%d} (parlab: %s) on the channel %s (obtain by %s) of the term %s (axiom %s)\n" 
        label 
	(display_parlab pl)
        (Term.display_term (Term.apply_substitution m_subst m_ch (fun t f -> f t)))
        (Recipe.display_recipe2 recipe_term_assoc Term.display_term r_ch)
        (Term.display_term (Term.apply_substitution m_subst t (fun t f -> f t)))
        (Recipe.display_axiom ax)
  | Input(label,r_ch,m_ch,r_t,m_t,pl) ->
      Printf.sprintf "Input {%d} (parlab: %s) on the channel %s (obtain by %s) of the term %s (obtain by %s)\n" 
        label 
	(display_parlab pl)
        (Term.display_term (Term.apply_substitution m_subst m_ch (fun t f -> f t)))
        (Recipe.display_recipe2 recipe_term_assoc Term.display_term r_ch)
        (Term.display_term (Term.apply_substitution m_subst m_t (fun t f -> f t)))
        (Recipe.display_recipe2 recipe_term_assoc Term.display_term r_t)
  | Comm(intern) ->
      Printf.sprintf "Internal communication between the input {%d} and output {%d}\n" intern.in_label intern.out_label  

let display_trace_no_unif symb_proc =
  let message_eq = Constraint_system.get_message_equations symb_proc.constraint_system in
  
  let subst = Term.unify message_eq in
  
  let trace = 
    List.fold_left (fun str_acc tr_label ->
      str_acc^(display_trace_label_no_unif subst symb_proc.axiom_name_assoc tr_label)
    ) "" (List.rev symb_proc.trace) in
  
  Printf.sprintf "%s\n%s\n" trace (Constraint_system.display symb_proc.constraint_system)

let display_trace_no_unif_no_csts symb_proc =
  let message_eq = Constraint_system.get_message_equations symb_proc.constraint_system in
  
  let subst = Term.unify message_eq in
  
  let trace = 
    List.fold_left (fun str_acc tr_label ->
      str_acc^(display_trace_label_no_unif subst symb_proc.axiom_name_assoc tr_label)
    ) "" (List.rev symb_proc.trace) in
  Printf.sprintf "%s" trace

let add_dependency_constraint sys variables axioms =
  let old = sys.constraint_system in
  let new_cst_system = Constraint_system.add_new_dependency_constraint old variables axioms in
  { sys with  constraint_system = new_cst_system }

(*************************************
	     Optimisation
**************************************)  

let is_same_input_output symb_proc1 symb_proc2 = 
  let rec add_sorted label = function
    | [] -> [label]
    | l::q when l < label -> l::(add_sorted label q)
    | l::q -> label::l::q
  in
  
  let switch_label_1 = ref []
  and switch_label_2 = ref [] in
  
  let rec same_trace = function
    | [], [] -> true
    | [], _ -> false
    | _,[] -> false
    | Output(l1,_,_,_,_,_)::q1, Output(l2,_,_,_,_,_)::q2 when l1 = l2 -> same_trace (q1,q2)
    | Input(l1,_,_,_,_,_)::q1, Input(l2,_,_,_,_,_)::q2 when l1 = l2 -> same_trace (q1,q2)
    | Comm({in_label = l1in; out_label = l1out})::q1,Comm({in_label = l2in; out_label = l2out})::q2 when l1in = l2in && l1out = l2out -> same_trace (q1,q2)
    | Input(l1,_,ch1,_,t1,_)::q1, Input(l2,_,ch2,_,t2,_)::q2 when Term.is_equal_and_closed_term ch1 ch2 && Term.is_equal_and_closed_term t1 t2 ->
        switch_label_1 := add_sorted l1 !switch_label_1;
        switch_label_2 := add_sorted l2 !switch_label_2;
        same_trace (q1,q2)
    | Output(l1,_,ch1,_,t1,_)::q1, Output(l2,_,ch2,_,t2,_)::q2 when Term.is_equal_and_closed_term ch1 ch2 && Term.is_equal_and_closed_term t1 t2 ->
        switch_label_1 := add_sorted l1 !switch_label_1;
      switch_label_2 := add_sorted l2 !switch_label_2;
        same_trace (q1,q2)
    | _,_ -> false
  in
  
  same_trace (symb_proc1.trace,symb_proc2.trace) && !switch_label_1 = !switch_label_2
 

(*************************************
      Annotated Semantics
**************************************)

(* Helping function for SSkeleton *)
let compare_term t1 t2 =
  try
    let n1,n2 = Term.name_of_term t1, Term.name_of_term t2 in
    Term.compare_name n1 n2
  with
  | _ -> Debug.internal_error ("[Process.ml >> SetOfSkeletons] I failed to compare to skeletons because, one channel is not a name")
	
type skeleton = InS of Term.term | OutS of Term.term					      

module Skeleton =
  struct
    type t = skeleton

    let compare sk1 sk2 =
      match (sk1,sk2) with
      | (InS t1, InS t2) -> compare_term t1 t2
      | (InS t1, OutS t2) -> 1
      | (OutS t1, InS t2) -> compare_term t1 t2
      | (OutS t1, OutS t2) -> -1

    let equal sk1 sk2 = (compare sk1 sk2) = 0
  end

module MapS =
struct
  include Map.Make(Skeleton)
end

let sk = function
  | In (ch,vx,p,l) -> Some (InS ch)
  | Out (ch, u, p, l) -> Some (OutS ch)
  | _ -> None

let equal_skeleton = Skeleton.equal

let display_sk = function
  | InS t -> "In_"^(Term.display_term t)
  | OutS t -> "Out "^(Term.display_term t)

let sk_of_symp symp = 
  try
    match (sk (fst(List.hd (symp.process)))) with
    | Some sk -> sk
    | None -> failwith "Will be handled."
  with
  | _ ->  (* Printf.printf "%s" (display_process (fst(List.hd symp.process))); *)
     Debug.internal_error "[process.ml >> sk_of_symp] A bad call to sk_of_symbp occurs. Should not be applied to non-reduced processes or empty process."

let need_labelise = function
  | (_,(_,(ToLabel))) -> true
  | _ -> false

let labelise symb_proc =
  let mapS = ref MapS.empty in
  (* go trough the list of processes and accumulate association sk -> l in mapS *)
  let rec labelise_procs n = function
    | pl :: q when not(need_labelise pl) -> pl :: (labelise_procs n q)
    | (p, l) :: q ->
       (match l with
       | (oldL, ToLabel) ->
	  let newL = n :: oldL in
	  let newLp = (newL, OkLabel) in
	  (match sk p with
	  | None ->  Debug.internal_error "[Process.ml >> labelise] I cannot labelise non-reduced processes."
	  | Some sk ->
	     begin
	       mapS := MapS.add sk newL !mapS;
	       (p, newLp) :: labelise_procs (n+1) q;
	     end)
       | _ ->   Debug.internal_error "[Process.ml >> labelise] I cannot labelise processes labelled with 'Dummy'.")
    | [] -> [] in
  
  let new_list_procs = labelise_procs 1 symb_proc.process in
  ({symb_proc with
     process = new_list_procs;
     (* the 'new' process has a focus only if it had a focus and mapS is empty (no new parallel compositions) *)
     has_focus = symb_proc.has_focus && MapS.is_empty !mapS;
   },
   !mapS)

exception Not_eq_left of string
exception Not_eq_right of string

let labelise_consistently mapS symb_proc =
  let mapSr = ref mapS in

  (*  go trough the list of processes and labeliszs using mapS *)
  let rec labelise_procs = function
    | ((p,l) as pl) :: q when not(need_labelise pl) ->
       (match sk p with
	| None ->  Debug.internal_error "[Process.ml >> labelise] I cannot labelise non-reduced processes."
	| Some skp ->
	   if MapS.mem skp !mapSr
	   then raise (Not_eq_right "Process on the right has a parallel composition with more processes that the one on the left. (1)")
	   else pl :: (labelise_procs q))
    | (p, l) :: q ->
       (match l with
	| (oldL, ToLabel) ->
	   (match sk p with
	    | None ->  Debug.internal_error "[Process.ml >> labelise] I cannot labelise non-reduced processes."
	    | Some skp ->
	       (try
		   let newL = MapS.find skp !mapSr in
		   if not(List.tl newL == oldL)
		   then raise (Not_eq_right ("Process on the right cannot answer with the same label (2). Here is the label: "^(display_parlab newL)^".\n"))
		   else begin
		       mapSr := MapS.remove skp !mapSr;
		       (p, (newL, OkLabel)) :: (labelise_procs q);
		     end
		 with
		 | Not_found -> raise (Not_eq_right "Process on the left has a parallel composition with more processes that the one on the right. (3)")))
 	| _ ->   Debug.internal_error "[Process.ml >> labelise] I cannot labelise processes labelled with 'Dummy'.")
    | [] -> [] in
  let was_empty = MapS.is_empty mapS in
  let new_list_procs = labelise_procs symb_proc.process in

  if not(MapS.is_empty !mapSr)
  then raise (Not_eq_right "Process on the left has a parallel composition with more processes that the one on the right. (4)")
  else {symb_proc with
	 process = new_list_procs;
	 has_focus = symb_proc.has_focus && was_empty; (* remain with focus only if it had a focus and no skeletons to labelise *)
       }
    
let list_of_choices_focus symP =
  let listChoices = ref [] in
  let rec buildList pre = function
    | [] -> ()
    | (proc, flagP) :: q ->
       (match sk proc with
	| None -> Debug.internal_error "[process.ml >> list_of_choices_focus] A bad call to sk occurs. Should not be applied to non-reduced processes or empty process."
	| Some sk -> let choice = {symP with
				    process = (proc,flagP) :: (pre @ q);
				    has_focus = true;
				  } in
		     listChoices := (choice, sk) :: !listChoices;
		     buildList ((proc,flagP) :: pre) q) in
  buildList [] (symP.process);
  !listChoices

let assemble_choices_focus listChoices symP = 
  (* returns a symbolic process equals to symP except that we moved a process of skeleton sk to the focus position *)
  let find_focus ske symP = 
    let rec search pre = function
      | ((proc,pl) as p) :: tl when ((sk proc) = (Some ske)) -> p :: (pre @ tl)
      | p :: tl -> search (p::pre) tl
      | [] -> raise (Not_eq_right ("Process on the right cannot answer to the left one by choosing a focused process with the same skeleton. Here is the skeleton: "^(display_sk ske)^".\n"))
    in
    let new_list_proc = search [] symP.process in
    { symP with
      process = new_list_proc;
      has_focus = true;
    }
  in
  List.map (fun (symP_left,ske) ->
	    let symP_right = find_focus ske symP in
	    (symP_left, symP_right))
	   listChoices


let has_focus symb_proc = symb_proc.has_focus

let is_improper symb_proc = symb_proc.is_improper

let set_focus new_flag symb_proc =
  { symb_proc with
    has_focus = new_flag;
    
  }

let display_symb_process symP =
  Printf.printf "##################### Symbolic Process ###########################\n";
  List.iter (fun (p,i) -> Printf.printf "## Process (index %d): %s\n" 0 (* i *) (display_process p))
	    symP.process;
  Printf.printf "%s" (display_trace_blocks symP)

let debug_f = ref false           (* Do we print debugging information ?  *)

(* ********************************************************************** *)
(*                 Test whether dependency constraints hold               *)
(* ********************************************************************** *)
let test_dependency_constraints symP =
  (* Test whether one dep. cst hold *)
  let rec test_cst frame r_subst = function
    | (_, []) -> true
    | ([], _) -> false
    | (r::lr , la) ->
       let r_t' = Recipe.apply_substitution r_subst r (fun t f -> f t) in
       if not(Recipe.is_closed r_t')
       (* It is better to first check that r :: lr do not contain any
      non-ground recipes ? *)
       then true                         (* cst is not ground *)
       else (
         if List.exists (fun ax -> Recipe.ax_occurs ax r) la
         then true                     (* cst hold thanks to r *)
         else test_cst frame r_subst (lr, la))
  in
  (* Scan the list of dep. csts*)
  let rec scan_dep_csts frame r_subst = function
    | [] -> true
    | ((lr, la) as cst) :: l ->
       (* We made the choice to firstly check the noUse criterion and then the closed recipe criterion.
	 Todo: find the most efficient order. *)
       if la <> [] && (false && (Constraint.is_subset_noUse la frame))     (* = la \susbseteq NoUse *)
       (* TODO: enlever le false et debugger is_subset_nouse *)
       then false
       else (if test_cst frame r_subst cst
	     then scan_dep_csts frame r_subst l
	     else false) in
  
  let csys = get_constraint_system symP in
  let frame = Constraint_system.get_frame csys in
  let recipe_eq = Constraint_system.get_recipe_equations csys in
  let r_subst = Recipe.unify recipe_eq in

  (* BEGIN DEBUG *)
  if !debug_f then begin
    let csts = (Constraint_system.get_dependency_constraints csys) in
    if csts <> [] then
      begin
	Printf.printf "### Symbolic Process: %s" (display_trace_no_unif symP);
	Printf.printf "We will check those dependency constraints: %s"
		      (Constraint_system.display_dependency_constraints csys);
	Printf.printf "Do those constraints hold?: ---- %B ----.\n\n" (scan_dep_csts frame r_subst csts);
      end;
		end;
  (* END DEBUG *)

  scan_dep_csts frame r_subst (Constraint_system.get_dependency_constraints csys)

(* Helping functions dealing with par_label *)
let rec listDrop l = function
  | 0 -> l
  | n -> listDrop (List.tl l) (n-1)

let extract_common l1 l2 =  
  let s1,s2 = List.length l1, List.length l2 in
  let s = min s1 s2 in
  let l1d, l2d = listDrop l1 (s1-s), listDrop l2 (s2-s) in
  let rec aux = function
    | (x1 :: tl1 as l1, x2 :: tl2 as l2) -> 
       if tl1 == tl2
       then (x1,x2,tl1)
       else aux (tl1,tl2)
    | _, _ ->   Debug.internal_error "[Process.ml >> extact_common] Should not happen!" in

  aux (l1,l2)

(* l1 ||^s l2 *)
let lab_inpar l1 l2 = 
  let x1,x2,ld = extract_common l1 l2 in
  if x1 <> x2
  then true
  else false 

(* l1 < l2 *)
let lab_ord l1 l2 =
  let x1,x2,ld = extract_common l1 l2 in
  x1 < x2

exception No_pattern

(* Recusrive functions that looks for a pattern given the par_label [par_lab] of the last block
   and  an accumulator of axioms [acc_axioms]*)
let rec search_pattern par_lab acc_axioms = function
  | [] -> raise No_pattern  
  (* block <|> par_lab: No_pattern *)
  | block :: trace when not(lab_inpar block.par_lab par_lab) -> raise No_pattern
  (* block || par_lab and <: acc+1 and rec call *)
  | block :: trace when lab_ord block.par_lab par_lab -> search_pattern par_lab (block.out @ acc_axioms) trace
  (* block || par_lab and >: return acc+1 (DEP CST) *)
  | block :: trace -> block.out @ acc_axioms


(* ********************************************************************** *)
(*                 Build dependency constraints given a symbolic process  *)
(* ********************************************************************** *)

let generate_dependency_constraints symP =
  if !debug_f then begin
		  pp "We are going to generate some dep. csts. on: ";
		  Printf.printf "%s\n" (display_trace_blocks symP);
		end;
  if List.length symP.trace_blocks < 2
  (* then only one block has been performed -> no constraint is added *)
  then symP
  else begin
      let block = List.hd symP.trace_blocks
      and trace = List.tl symP.trace_blocks in
      let list_recipes = block.inp in
      try 
	let list_axioms = search_pattern block.par_lab [] trace in
	let new_sys  = add_dependency_constraint symP list_recipes list_axioms in
	
	(* BEGIN DEBUG *)
	if !debug_f
	then begin
	    let dep_cst = Constraint_system.display_dependency_constraints
			    (get_constraint_system new_sys) in
            Printf.printf "---------------------- A Dependency constraint HAS BEEN ADDED----------------------\n### Dependency constraints: %s\n" dep_cst;
            Printf.printf "### Symbolic Process: %s\n" (display_trace_no_unif new_sys);
	  end;
	(* END DEBUG *)
	
	new_sys
      with
      | No_pattern -> symP;
    end
