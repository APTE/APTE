(** Operations on terms *)

(** {% This module regroups all the functions that manipulate terms. In~\thesis, the terms
    are splitted into first (resp. second) order terms called messages (resp. recipe). 
    In this module, we focus on the messages. The recipe are handled in a different module. %}*)

(** {2 Symbol} *)

(** A symbol can be a destructor or a constructor.*)

(** The type [symbol] represents the type of function symbol.*)
type symbol
  
(** {3 Built-in signature} *)

(** {% The algorithm described in~\thesis~considers a fix set of cryptographic primitives
    whose behaviour is defined by rewrite rules plus any number of constructors. Thus, we directly defined
    here this set of cryptographics primitives. %} 
    
*)    
    
(** {4 Built-in constructors} *)    

(** [senc] is the symbol for symmetric encryption (arity 2). *)
val senc : symbol 

(** [aenc] is the symbol for asymmetric encryption (arity 2). *)
val aenc : symbol

(** [pk] is the symbol for asymmetric public key (arity 1). *)
val pk : symbol

(** [vk] is the symbol for public verification key used in signature (arity 1). *)
val vk : symbol

(** [sign] is the symbol for asymmetric public key (arity 1). *)
val sign : symbol

(** [hash] is the symbol for hash function (arity 1). *)
val hash : symbol

(** {4 Built-in destructors} *)

(** [sdec] is the symbol for symmetric decryption (arity 2). *)
val sdec : symbol

(** [adec] is the symbol for asymmetric decryption (arity 2). *)
val adec : symbol

(** [checksign] is the symbol for signature verification (arity 2). *)
val checksign : symbol

(** Although the algorithm described {% in~\thesis~%} only have a pair function of arity 2 with its
    associated projection, it can be extended to tuple of any arity. Thus, a user we be allowed to 
    use such tuple. *) 

(** [nth_projection f i] returns the projection function symbol of the [i]{^ th} element of tuple function symbol [f].
    Note that for a tuple of arity [n], the range of [i] is [1...n]. 
    
    @raise Internal_error if [f] is not a tuple.
    @raise Not_found if [f] was not previously introduced by [get_tuple].*)    
val nth_projection : symbol -> int -> symbol

(** [get_projections f] returns the list [[f_1;...;f_n]] with [f_i] is the projection 
    function symbol of the [i]{^ th} element of the tuple function symbol [f].
    It returns the same result as [[nth_projection f 1; ...; nth_projection f n]].
    
    @raise Internal_error if [f] is not a tuple.
    @raise Not_found if [f] was not previously introduced by [get_tuple].*)
val get_projections : symbol -> symbol list

(** The list contains all tuples introduced by the algorithm. *)    
val all_tuple : symbol list ref 

(** The list of all constructors (included the tupple function symbol) used in the algorithm.*)
val all_constructors : symbol list ref

(** The number of constructors used in the algorithm. *)
val number_of_constructors : int ref

(** {3 Addition} *)

(** [new_symbol ar s] creates a constructor function symbol with the name [s] and the arity [ar].
    The resulting symbol is automatically added into [all_constructors]. 
    Moreover, [number_of_constructors] is increased by 1.
    Note that if the constructor is in fact a tuple, it is better to use [get_tuple].*)
val new_constructor : int -> string -> symbol

(** [get_tuple ar] get the function symbol for tuple of arity [ar].
    If such function symbol was not created yet, it creates it and 
    the resulting symbol is automatically added into [all_constructors].
    Moreover, [number_of_constructors] is increased by 1.
    At last, the associated projection function symbol are automatically added into [all_projection].*)
val get_tuple : int -> symbol

(** {3 Symbol testing} *)

(** [is_equal_symbol f1 f2] returns [true] iff [f1] and [f2] are the same function symbol.*)
val is_equal_symbol : symbol -> symbol -> bool

(** [is_tuple f] returns [true] iff [f] is a tuple. *)
val is_tuple : symbol -> bool

(** [is_constructor f] returns true iff [f] is a constructor or a tuple. Note that all tuples are constructors. *)
val is_constructor : symbol -> bool

(** [is_destructor f] returns true iff [f] is a destructor. *)
val is_destructor : symbol -> bool

(** {3 Symbol Access} *)

(** [get_arity f] returns the arity of the function symbol [f].*)
val get_arity : symbol -> int

(** {3 Symbol Display} *)

val display_symbol_without_arity  : symbol -> string

val display_symbol_with_arity  : symbol -> string

(** {2 Messages} *)

(** The type [quantifier] is associated to a variable to quantify it.*)
type quantifier = 
  | Free
  | Existential
  | Universal

(** A [variable] is always quantified. {% It corresponds to the set $\Xun$ in~\thesis. %} *)  
type variable

(** A [name] is can be either public or private. *)
type name_status = 
  | Public
  | Private
  
(** The type [name] corresponds to the set {% $\N$ in~\thesis. %} *)
type name

(** The type [term] corresponds to the set {% $\T(\F,\N \cup \Xun)$ in~\thesis. %} *)
type term

(** {3 Variable generation} *)

(** The variables created by the functions below are structuraly and physically different *)

(** [fresh_variable q] creates a fresh variable quantified by [q].*)
val fresh_variable : quantifier -> variable

(** [fresh_variable_from_id q s] creates a fresh variable quantified as [q] with display identifier [s].*)
val fresh_variable_from_id : quantifier -> string -> variable

(** [fresh_variable_from_var v] creates a fresh variable 
    with the same display identifier and quantifier as the variable [v].*)
val fresh_variable_from_var : variable -> variable

(** [fresh_variable_list q n] creates a list of [n] fresh variables all quantified as [q].*)
val fresh_variable_list : quantifier -> int -> variable list

(** [fresh_variable_list2 q n] creates a list of [n] fresh variables all quantified as [q] and considered as terms.*)
val fresh_variable_list2 : quantifier -> int -> term list

(** {3 Name generation} *)

(** [fresh_name ns] creates a fresh name with the status [ns].*)
val fresh_name : name_status -> name

(** [fresh_name_from_id ns s] creates a fresh name with status [ns] and  with display identifier [s].*)
val fresh_name_from_id : name_status -> string  -> name

(** [fresh_name_from_name n] creates a fresh name with the same display identifier
    and same status as [n].*)
val fresh_name_from_name : name -> name

(** {3 Generation of terms} *)

(** [term_of_variable v] returns the variable [v] considered as a term.*)
val term_of_variable : variable -> term

(** [term_of_name n] returns the name [n] considered as a term.*)
val term_of_name : name -> term

(** [variable_from_term t] returns the term [t] as a variable. 
    @raise Internal_error if [t] is not a variable. *)
val variable_of_term : term -> variable

(** [name_from_term t] returns the term [t] as a name.
    @raise Internal_error if [t] is not a name. *)
val name_of_term : term -> name

(** [apply_function f args] applies the the function symbol [f] to the arguments [args].
    If [args] is the list [[t1;...;tn]] then the term obtained is [f(t1,...,tn)].
    
    {% \lowdebug %} Raise an internal error if the number of arguments in [args] does not coincide
    with the arity of [f].*)
val apply_function : symbol -> term list -> term

(** [rename v_list n_list t] creates a new term from [t] where each [v_i] is replaced by [v'_i]
    and each [n_i] is replaced by [n'_i] where [v_list] is the list [(v_1,v'_1),...,(v_p,v'_p)]
    and [n_list] is the list [(n_1,n'_1),...,(n_q,n'_q)].*)
val rename : (variable * variable) list -> (name * name) list -> term -> term    
    
(** {3 Access functions} *)

(** [top t] returns the symbol at the root position of [t]. 
    @raise Internal_error if [t] is not a function symbol application.*)
val top : term -> symbol
    
(** [nth_args t i] returns the [i]{^ th} argument of the constructed term [t].  
    Note that the index [i] start with 1 and not 0. For example, if [t] is the term {% $f(t_1,\ldots t_n)$ %}
    then [nth_args t i] returns the term {% $t_i$ %}.
    @raise Internal_error if [t] is not a function symbol application.*)
val nth_args : term -> int -> term

(** [get_args t] returns the list of argument of the constructed term [t]. For example, if [t] is the term {% $f(t_1,\ldots t_n)$ %}
    then [get_args t] returns the list {% [$t_1$;\ldots;$t_n$] %}.
    @raise Internal_error if [t] is not a function symbol application.*)
val get_args : term -> term list

(** [get_quantifier v] returns the quantifier of [v].*)
val get_quantifier : variable -> quantifier
    
(** {3 Scanning} *)

(** [occurs v t] returns [true] iff the variable [v] occurs in the term [t].*)
val var_occurs : variable -> term -> bool

(** [occurs_list v_list t] returns [true] iff one of the variable in [v_list] occurs in the term [t]. *)
val var_occurs_list : variable list -> term -> bool

(** [exists_var q t] returns [true] iff there exists a variable quantified as [q] in the term [t]. *)
val exists_var : quantifier -> term -> bool

(** [for_all_var q t] returns [true] iff all variables in the term [t] are quantified as [q]. *)
val for_all_var : quantifier -> term -> bool

(** [exists_name s t] returns [true] iff there exists a name in [t] with status [s]. *)
val exists_name_with_status : name_status -> term -> bool

(** [exists_name t] returns [true] iff there exists a name in [t]. *)
val exists_name : term -> bool

(** [is_equal_term t1 t2] returns [true] iff the terms [t1] and [t2] are equal. *)
val is_equal_term : term -> term -> bool

(** [is_equal_term t1 t2] returns [true] iff the terms [t1] and [t2] are equal. *)
val is_equal_and_closed_term : term -> term -> bool

(** [is_equal_name n1 n2] returns [true] iff the name [n1] and [n2] are equal. *)
val is_equal_name : name -> name -> bool

(** [is_variable t] returns [true] iff the term [t] is a variable. *)
val is_variable : term -> bool

(** [is_name t] returns [true] iff the term [t] is a name. *)
val is_name : term -> bool

(** [is_name_status s t] returns [true] iff the term [t] is a name with status [s]. *)
val is_name_status : name_status -> term -> bool

(** [is_function t] returns [true] iff the term [t] is a function symbol application. *)
val is_function : term -> bool

(** [is_constructor_term t] returns [true] iff {% $t \in \T(\Fc, \Xun \cup \N)$. %} *)
val is_constructor_term : term -> bool

(** {3 Iterators} *)

(** [fold_left_args f acc t] is [f (...(f (f acc t1) t2) ...) tn] if [t] is
    the term {% $g(t_1,...,t_n)$ %} for some function symbol {% $g$ %}.
    @raise Internal_error if [t] is not a function application.*) 
val fold_left_args : ('a -> term -> 'a) -> 'a -> term -> 'a

(** [fold_right_args f t acc] is [f t1 (f t2 (...(f tn acc)...))] if [t] is
    the term {% $g(t_1,...,t_n)$ %} for some function symbol {% $g$ %}.
    @raise Internal_error if [t] is not a function application.*)
val fold_right_args : (term -> 'a -> 'a) -> term -> 'a -> 'a

(** [map_args f t] is the list [[f t1; ...; f tn]] if [t] is
    the term {% $g(t_1,...,t_n)$ %} for some function symbol {% $g$ %}.
    @raise Internal_error if [t] is not a function application.*)
val map_args : (term -> 'a) -> term -> 'a list

(** [fold_left_args2 f acc t l] is [f (...(f (f acc t1 e1) t2 e2) ...) tn en] if [t] is
    the term {% $g(t_1,...,t_n)$ %} for some function symbol {% $g$ %}
    and [l] is the list [[e1;...;en]].
    @raise Internal_error if [t] is not a function application.*) 
val fold_left_args2 : ('a -> term -> 'b -> 'a) -> 'a -> term -> 'b list -> 'a

(** {3 Display} *)

val display_term : term -> string

val display_name : name -> string

val display_variable : variable -> string

(** {2 Mapping table} *)

module VariableMap :
sig
  (** ['a map] is the type that represents the mapping of variable to element of type ['a]. *)
  type 'a map
  
  (** [empty] is the empty mapping function. *)
  val empty : 'a map
  
  (** [is_empty map] returns [true] iff [map] is empty. *)
  val is_empty : 'a map -> bool
  
  (** [add v elt map] returns a map containing the same bindings as [map], 
      plus a binding of [v] to [elt]. If [v] was already bound in [map], 
      its previous binding disappears. *)
  val add : variable -> 'a -> 'a map -> 'a map
  
  (** [find v map] returns the current binding of [v] in [map].
      @raise Not_found if no binding exists. *)
  val find : variable -> 'a map -> 'a
  
  (** [mem v map] returns [true] iff [map] contains a binding for [v].*)
  val mem : variable -> 'a map -> bool
  
  val display : ('a -> string) -> 'a map -> unit
end

(** {2 Substitution} *)

type substitution

(** [identity] corresponds to the identity substitution.*)
val identity : substitution

(** [is_identity s] returns [true] iff [s] is the identity substitution. *)
val is_identity : substitution -> bool

(** [create_substitution v t] creates the substitution {% $\{v \rightarrow t\}$ %}.*)
val create_substitution : variable -> term -> substitution

(** [compose] {% $\sigma_1$~$\sigma_2$ %} returns the substitution {% $\sigma_1\sigma_2$ %}.

    {% \lowdebug %} Raise an internal error if the domain of two substitutions are not disjoint.*)
val compose : substitution -> substitution -> substitution

(** [filter_domain f s] returns the substitution [s] restricted to variables that satisfy [f]. *) 
val filter_domain : (variable -> bool) -> substitution -> substitution    

(** [apply_substitution subst elt map_elt] applies the substitution [subst] on the element [elt]. The function
    [map_elt] should map the terms contained in the element [elt] on which [subst] should be applied.
    
    For example, applying a substitution [subst] on a list of terms [term_list] 
    could be done by applying [apply_substitution subst term_list (fun l f -> List.map f l)].
    
    Another example: applying a substitution [subst] on the second element of a couple of terms could be 
    done by applying [apply_substitution subst term_c (fun (t1,t2) f -> (t1, f t2))].
    *)
val apply_substitution : substitution -> 'a -> ('a -> (term -> term) -> 'a) -> 'a

(** [apply_substitution_change_detected subst elt map_elt] is similar to [apply_substitution] 
    except that the function [map_elt], which should map the term to be substituted, will consider
    a function that returns if a term was modify or not. 
    
    [apply_substitution_change_detected subst elt map_elt] is faster but has the same result as 

{[apply_substitution subst elt (fun a f -> 
  map_elt a (fun t ->
    let t' = f t in not (is_equal_term t t'),t'))]}
*)
val apply_substitution_change_detected : substitution -> 'a -> ('a -> (term -> bool * term) -> 'a) -> 'a
  
(** [equations_of_substitution s] returns the list [[(x1,t1);...;(xn,tn)]] where 
    [s] is the substitution {% $\{x_1 \rightarrow t_1, \ldots, x_n \rightarrow t_n\}$ %}.*)
val equations_of_substitution : substitution -> (term * term) list  
  
(** {2 Rewrite rules} *)

(** [fresh_rewrite_rule f] returns the couple [([t1,...,tn],t)] where {% $f(t_1,\dots,t_n) \rightarrow t$ %}
    is a fresh rewrite rule of [f].*)
val fresh_rewrite_rule : symbol -> term list * term

(** [link_destruc_construc s_d s_c] returns [true] iff [s_d] is the destructor symbol of the constructor symbol [s_c].
    @raise Internal_error if [s_c] is not a constructor or if it is a tuple symbol.
    
    Example : [link_destruc_construc sdec senc] returns [true].*)
val link_destruc_construc : symbol -> symbol -> bool

(** [constructor_to_destructor s_c] returns the destructor symbol of the constructor symbol [s_c].
    @raise Internal_error if [s_c] is not a constructor or if it is a tuple symbol.*)
val constructor_to_destructor : symbol -> symbol

(** {2 Unification} *)

exception Not_unifiable

(** [unify l] unifies the pairs of term in [l] and returns the substitution that unifies them
    @raise Not_unifiable if no unification is possible. *)
val unify : (term * term) list -> substitution

(** [is_unifiable l] returns [true] iff the pairs of term in [l] are unifiable.*)      
val is_unifiable : (term * term) list -> bool
    
(** [unify_and_apply l elt map_elt] unifies the pairs of term in [l] and apply the substitution that unifies them
    on the terms in [elt] according to the function [map_elt].
    @raise Not_unifiable if no unification is possible.
    
    It is faster but returns the same as [apply_substitution (unify l) elt map_elt].*)
val unify_and_apply : (term * term) list -> 'a -> ('a -> (term -> term) -> 'a) -> 'a

(** [unify_and_apply_change_detected l elt map_elt] unifies the pairs of term in [l] and apply the substitution that unifies them
    on the terms in [elt] according to the function [map_elt].
    @raise Not_unifiable if no unification is possible.
    
    It is faster but returns the same as [apply_substitution_change_detected (unify l) elt map_elt].*)
val unify_and_apply_change_detected : (term * term) list -> 'a -> ('a -> (term -> bool * term) -> 'a) -> 'a

(** [unify_modulo_rewrite_rules l] unifies the pairs of term in [l] modulo the rewriting systems.
    All variables introduced by the unification are quantified existentially.
    @raise Not_unifiable if no unification is possible or if a destructor cannot be reduced *)
val unify_modulo_rewrite_rules : (term * term) list -> substitution

(** [unify_modulo_rewrite_rules_and_apply l elt map_elt] unifies the pairs of term in [l] modulo the rewriting systems
    and apply the substitution that unifies them on the terms in [elt] according to the function [map_elt].
    All variables introduced by the unification are quantified existentially.
    @raise Not_unifiable if no unification is possible.
    
    It is faster but returns the same as [apply_substitution (unify l) elt map_elt].*)
val unify_modulo_rewrite_rules_and_apply : (term * term) list -> 'a -> ('a -> (term -> term) -> 'a) -> 'a

(** {2 Formula} *)

(** The type [formula] represents a disjunction of inequation of the form {% $\forall \tilde{x}. \bigvee_{i = 1}^n  u_i \neqi v_i$
    for some terms $u_i$,$v_i$ that may contain destructor symbol. Note that the semantics of $u \neqi v$ is given in~\thesis.%} *)
type formula

(** [top_formula] is the always true formula.*)
val top_formula : formula

(** [bottom_formula] is the always false formula.*)
val bottom_formula : formula

(** [create_inequation t_1 t_2] creates the formula {% $t_1 \neqi t_n$. %}
    Note that the quantifier of the variables are not modified. For instance, the existential vairiables
    in [t_1] and [t_2] do not become universal variables.
    Note that [create_inequation] is not commutative, i.e. [create_inequationt t_1 t_2] is different from
    [create_inequation t_2 t_1].*)
val create_inequation : term -> term -> formula

(** [create_disjunction_inequation [(u_1,v_1);...;(u_n,v_n)]] creates the formula {% $\bigvee_{i=1}^n u_i \neqi v_i$. %}
    Note that the quantifier of the variables are not modified. *)
val create_disjunction_inequation : (term * term) list -> formula

       
(** {3 Iterators} *)    
    
(** [iter_inequation_formula f phi] is [f u1 v1; f u2 v2; ...; f un vn] where [phi] is the formula
    {% $\forall \tilde{x}. \bigvee_{i = 1}^n  u_i \neqi v_i$ %}.*)
val iter_inequation_formula : (term -> term -> unit) -> formula -> unit    

(** [map_term_formula phi f ] is the formula [create_disjunction_inequation [(f u1,f v1);...;(f un,f vn)]]
    where [phi] is the formula {% $\forall \tilde{x}. \bigvee_{i = 1}^n  u_i \neqi v_i$ %}.*)
val map_term_formula : formula -> (term -> term) -> formula

(** Similar to [map_term_formula] except that it returns the couple [(b,phi')] where [phi'] is the 
   formula [phi] on which we applied [f] and [b] is [true] iff one of the application of [f] returned
   true.*)
val map_term_formula_change_detected : formula -> (term -> bool * term) -> bool * formula

(** {3 Formula scanning} *)

(** [find_and_apply_formula f_test f_apply f_no formula] searches in [formula] an inequation satisfying [f_test]. 
   If such inequation exists then it applies [f_apply] on it else it apply the function [f_no].
   
   Note that since an inequation {% $u \neqi v$ %} is semantically the same as {% $v \neqi u$ %}, it is recommanded that
   [f_test u v] and [f_test v u] are equal. Same for [f_apply]. *)
val find_and_apply_formula : (term -> term -> bool) -> (term -> term -> 'a) -> (unit -> 'a) -> formula -> 'a
 
(** [is_bottom formula] returns [true] iff [formula] is the always false formula.*)
val is_bottom : formula -> bool

(** [is_true formula] returns [true] iff [formula] is the always true formula.*)
val is_top : formula -> bool

(** [is_in_formula t_1 t_2 formula] returns [true] iff [formula] is of the form {% $\forall \tilde{x}. t_1 \neqi t_2 \vee F$
    where $F$ is a disjunction of inequation.%} 
    Note that this function is commutative, i.e. [is_in_formula t_1 t_2 phi] is the same as [is_in_formula t_2 t_1 phi].*)
val is_in_formula : term -> term -> formula -> bool

    
(** {3 Simplification} *) 

(** {% Following~\thesis, a substitution $\sigma$ of constructor terms models a formula $u \neqi v$, denoted $\sigma \vDash_c u \neqi v$, if
    $u\sigma\mydownarrow \neq v\sigma\mydownarrow$ or $\valid{u}$ or $\valid{v}$.
    $\vDash_c$ is naturally extended to formula $\forall \tilde{x}. \bigvee_i u_i \neqi v_i$.
    The simplification functions in this section preserve the models of the formulas.%}   
*)

(** {% This function transforms a formula of the form $\forall \tilde{x} \bigvee_i u_i \neq v_i$ into 
    a formula of the form $\forall \tilde{y} \bigvee_j x_j \neqi t_j$ where $u_i,v_i,t_i$ are all constructors terms
    and all $x_j$ are distinct.%} *)    
val simplify_formula : formula -> formula

(** {% This function simplifies a formula containing only constructor term by the simplification rules 
     defined in \thesisL{Figure 7.3}. %} *)
val simplify_formula_phase_2 : formula -> formula

(** This function simplifies a formula that may contain destructor symbols into a formula that contains only constructor terms.*)    
val simplify_formula_modulo_rewrite_rules : formula -> formula

(** {3 Display} *)

val display_formula : formula -> string
