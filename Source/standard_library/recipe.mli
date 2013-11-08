(*************************************************************************
** APTE v0.3.2beta - Algorithm for Proving Trace Equivalence            **
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

(** Operations on recipes *)

(** This module regroups all the functions that manipulate recipes. In {% ~\thesis %}, the terms
    are splitted into first (resp. second) order terms called messages (resp. recipe). 
    In this module, we focus on the recipes. The message are handled in a different module.
    In theory a recipe and a message are both terms hence one could consider this module almost 
    as a copy of the module [Term]. However, in the algorithm presented in {% ~\thesis %}, the usage of
    message and recipe are really different. *)

(** {2 Recipe} *)

(** The type [variable] corresponds to the set {% $\Xdeux$ in~\thesis. %}
    Since the recipe variable are always introduced in the algorithm with a deducibility constraint, 
    a recipe variable is always associated to an integer called the support in {% ~\thesis %}. 
    For example, if {% $X, i \deduce u$ is a deducibility constraint, the support of $X$ is $i$.%}
    Hence a variable is always associated to a support in our module *)
type variable

(** The type [axiom] corresponds to the set {% $\AX$ in~\thesis. Similarly to the variable, a axiom 
    is always associated to a support. In~\thesis, for an axiom $\ax_i$, $i$ is the support.%} *)
type axiom

(** The type [recipe] corresponds to the set {% $\T(\F, \AX \cup \Xdeux)$ in~\thesis. Note that the recipes
    does not have names. It corresponds to the recipes used in Chapter 7 and 8 of~\thesis. %} *)
type recipe

(** {3 Fresh function} *)

(** [fresh_variable n] creates a fresh variable with support [n].*)
val fresh_variable : int -> variable

(** [fresh_variable_from_id s n] creates a fresh variable with support [n] and display identifier [s].*)
val fresh_variable_from_id : string -> int -> variable

(** [fresh_variable_list nb n] creates a list of [nb] fresh variables all with support [n].*)
val fresh_variable_list : int -> int -> variable list

(** [fresh_variable_list2 nb n] creates a list of [nb] fresh variables considered as recipes and all with support [n].*)
val fresh_variable_list2 : int -> int -> recipe list

(** [fresh_free_variable n] creates a fresh free variable with support [n]. *)
val fresh_free_variable : int -> variable

(** [fresh_free_variable_from_id s n] creates a fresh free variable with support [n] and display identifier [s].*)
val fresh_free_variable_from_id : string -> int -> variable

(** [fresh_free_variable_list nb n] creates a list of [nb] fresh free variables all with support [n].*)
val fresh_free_variable_list : int -> int -> variable list

(** [axiom n] creates an axiom with support [n].*)
val axiom : int -> axiom

(** {3 Generation of recipe} *)

(** [recipe_of_variable v] returns the variable [v] considered as a recipe. *)
val recipe_of_variable : variable -> recipe

(** [recipe_of_axiom ax] returns the axiom [ax] considered as a recipe. *)
val recipe_of_axiom : axiom -> recipe

(** [variable_of_recipe r] returns the recipe [r] as a variable.
    @raise Internal_error if [r] is not a variable.*)
val variable_of_recipe : recipe -> variable

(** [get_variables_of_recipe r] returns the list of variables in [r]. *)
val get_variables_of_recipe : recipe -> variable list

(** [axiom_of_recipe r] returns the recipe [r] as an axiom.
    @raise Internal_error if [r] is not an axiom.*)
val axiom_of_recipe : recipe -> axiom

(** [apply_function f args] applies the the function symbol [f] to the arguments [args].
    If [args] is the list [[r1;...;rn]] then the recipe obtained is [f(r1,...,rn)].
    
    {% \lowdebug %} Raise an internal error if the number of arguments in [args] does not coincide
    with the arity of [f].*)
val apply_function : Term.symbol -> recipe list -> recipe

(** {3 Access} *)

(** [top r] returns the symbol at the root position of [r]. 
    @raise Internal_error if [r] is not a function symbol application.*)
val top : recipe -> Term.symbol

(** [get_support v] returns the support of the variable [v]. *)
val get_support : variable -> int

(** {3 Testing} *)

(** [is_equal_variable v1 v2] returns [true] iff [v1] and [v2] are the same variable.*)
val is_equal_variable : variable -> variable -> bool

(** [is_equal_axiom ax1 ax2] returns [true] iff [ax1] and [ax2] are the same axioms.*)
val is_equal_axiom : axiom -> axiom -> bool

(** [is_equal_recipe r1 r2] returns [true] iff [r1] and [r2] are the same recipes.*)
val is_equal_recipe : recipe -> recipe -> bool

(** [occurs v r] return true iff the variable [v] is in the recipe [r] *)
val occurs : variable -> recipe -> bool

(** [is_free_variable v] returns [true] iff [v] is free.*)
val is_free_variable : variable -> bool

(** [is_free_variable2 r] returns [true] iff [r] is a free variable. *)
val is_free_variable2 : recipe -> bool

(** [is_variable r] returns [true] iff [r] is a variable. *)
val is_variable : recipe -> bool

(** [is_axiom r] returns [true] iff [r] is an axiom. *)
val is_axiom : recipe -> bool

(** [is_function r] returns [true] iff [r] is a function symbol application. *)
val is_function : recipe -> bool

(** {3 Iterators} *)

(** [iter_args f r] is [f r1; ...; f rn] if [r] is
    the recipe {% $g(r_1,...,r_n)$ %} for some function symbol {% $g$. %}
    @raise Internal_error if [r] is not a function application.*)
val iter_args : (recipe -> unit) -> recipe -> unit

(** [map_args f r] is the list [[f r1; ...; f rn]] if [r] is
    the recipe {% $g(r_1,...,r_n)$ %} for some function symbol {% $g$. %}
    @raise Internal_error if [r] is not a function application.*)
val map_args : (recipe -> 'a) -> recipe -> 'a list

(** {3 Display} *)

val display_variable : variable -> string

val display_axiom : axiom -> string

val display_recipe : recipe -> string

(** [display_recipe assoc f_display r] display the recipe [r] but each variable
    and axiom [r'] in [r] is displayed as [f_display b] if [(r',b)] is in [assoc]
    else is normally displayed.*) 
val display_recipe2 : (recipe * 'a) list -> ('a -> string) -> recipe -> string


(** {2 Variable Mapping} *)

module VariableMap :
sig
  (** ['a map] is the type that represents the mapping of variable to element of type ['a]. *)
  type 'a map
  
  (** [empty] is the empty mapping function. *)
  val empty : 'a map
  
  (** [is_empty map] returns [true] iff [map] is empty. *)
  val is_empty : 'a map -> bool
  
  (** [add v elt map] returns a map containing the same bindings as [map], 
      plus a binding of [v] to [elt].
If [v] was already bound in [map], 
      its previous binding disappears. *)
  val add : variable -> 'a -> 'a map -> 'a map
  
  (** [find v map] returns the current binding of [v] in [map].
      @raise Not_found if no binding exists. *)
  val find : variable -> 'a map -> 'a
  
  (** [mem v map] returns [true] iff [map] contains a binding for [v].*)
  val mem : variable -> 'a map -> bool
end


(** {2 Substitution and unify} *)

(** [substitution] corresponds to a mapping from {% $\Xdeux$ to $\T(\F, \AX \cup \Xdeux)$. %} *)
type substitution

(** [is_identity s] returns [true] iff [s] is the identity substitution. *)
val is_identity : substitution -> bool

(** [unify l] returns the most general unifier of the pairs of recipes in [l].*)
val unify : (recipe * recipe) list -> substitution

(** [create_substitution v r] returns the substitution {% $\\{v \mapsto r\\}$. %} *)
val create_substitution : variable -> recipe -> substitution

(** [create_substitution2 v r] returns the substitution {% $\\{v \mapsto r\\}$. %} 
    @raise Internal_error if [v] is not a variable.*)
val create_substitution2 : recipe -> recipe -> substitution

(** [apply_substitution subst elt map_elt] applies the substitution [subst] on the element [elt]. The function
    [map_elt] should map the recipes contained in the element [elt] on which [subst] should be applied.
    See [Term.apply_substitution] for more explanation. *)
val apply_substitution : substitution -> 'a -> ('a -> (recipe -> recipe) -> 'a) -> 'a
  
(** [equations_from_substitution subst] returns [[(v1,r1);...;(vn,rn)]]
    if [subst] is the substitution {% $\\{ v_1 \mapsto r_1, \ldots, v_n \mapsto r_n \\}$. %}*)
val equations_from_substitution : substitution -> (recipe * recipe) list

(** [filter_domain f s] returns the substitution [s] restricted to variables that satisfy [f]. *) 
val filter_domain : (variable -> bool) -> substitution -> substitution    


(** {2 Path} *)

(** The [path] corresponds to the path of a recipe defined in {% ~\thesisL{Definition 7.4}.
    It corresponds to the set $\Fd^* \cdot \AX$ in~\thesis.%} *)
type path

(** [path_of_recipe xi] returns the path of a recipe. It corresponds to {% $\mpath(\xi)$ in~\thesis~
    where $\xi$ is a recipe.%}
    @raise Internal_error if the path of [xi] is not closed or if the path if not defined. *)
val path_of_recipe : recipe -> path

(** [apply_function_to_path f p] returns the path {% $f \cdot p$. %} *)
val apply_function_to_path : Term.symbol -> path -> path

(** [axiom_path ax] returns the path [ax]. *)
val axiom_path : axiom -> path

(** {3 Testing path} *)

(** [is_equal_path p1 p2] returns [true] iff [p1] and [p2] are the same path. *)
val is_equal_path : path -> path -> bool

(** [is_recipe_same_path r1 r2] returns [true] iff the paths of [r1] and of [r2] are the same. 
    Note that two recipes having the same path does not imply that the recipes are equal.*)
val is_recipe_same_path : recipe -> recipe -> bool

(** [is_path_of_recipe r p] returns [true] iff the path of [r] is [p]. *)
val is_path_of_recipe : recipe -> path -> bool 

(** {3 Display} *)

val display_path : path -> string

(** {2 Recipe context} *)

(** The type [context] corresponds to the set {% $\T(\Fc, \Fd^* \cdot \AX \cup \Xdeux)$ in~\thesis.
    The context of a recipe, defined in~\thesisL{Definition 7.6}, %} is used in the algorithm 
    for dealing with the inequations. *)
type context

(** [context_of_recipe r] returns the context of the recipe [r] following {% ~\thesisL{Definition 7.6}. 
    Note that in this definition, a frame is needed as parameter. But since we consider context with only
    constructor function symbol as application function, such frame is not necessary. %} *)
val context_of_recipe : recipe -> context

(** [recipe_of_context c] transforms the context [c] as a recipe if [c] is included in {% $\T(\F, \Xdeux)$. %}
    [c] cannot contain a path since one cannot reconstruct a recipe from a path.
    @raise Internal_error if [c] is not included in {% $\T(\F, \Xdeux)$. %} *)
val recipe_of_context : context -> recipe

val path_of_context : context -> path

val top_context : context -> Term.symbol

(** [apply_substitution_on_context theta elt map_elt] first transforms the substitution {% $\theta = \\{X_i \mapsto \xi_i\\}_i$
    into a substitution $\theta' = \\{X_i \mapsto \gamma_i\\}_i$ %} where [gamma_i] is the result of [context_of_recipe xi_i].
    Then it applies the substitution [theta'] on the [elt]. The function
    [map_elt] should map the contexts contained in the element [elt] on which [theta'] should be applied.*)
val apply_substitution_on_context : substitution -> 'a -> ('a -> (context -> context) -> 'a) -> 'a

(** {3 Testing} *)

(** [is_variable_context c] returns [true] iff [c] is a variable, i.e. {% is in $\Xdeux$. %} *)
val is_variable_context : context -> bool

(** [is_path_context c] returns [true] iff [c] is a path, i.e. {% is in $\F^* \cdot \AX$. %} *)
val is_path_context : context -> bool

(** [is_closed_context c] returns [true] iff [c] is closed, i.e. {% is in $\T(\F,\F^* \cdot \AX)$. %} *)
val is_closed_context : context -> bool

(** [is_closed_context c] returns [true] iff there exists a path subterm of [c]. *)
val exists_path_in_context : context -> bool

(** {3 Access} *)

(** [get_max_param_context c] returns the maximal parameter of the recipe context [c], defined {% in~\thesisL{Section 7.4.2.2}
    and denoted $\maxparamC{\C}{c}$ where $\C$ is a constraint system. %} Note that our function does not have 
    a constraint system as argument. Indeed, the purpose of the constraint system is to allow the association support/variable
    {% in~\thesis~%} which is coded directly in the variables in this module. *)
val get_max_param_context : context -> int

(** {3 Display} *)

val display_context : context -> string

(** {2 Formula on contexts of recipes} *)


(** The type [formula] correspond to a disjunction of inequation between context of recipe.
    It corresponds to the formulas contains in the association table {% in~\thesisL{Section 7.4.2.2}. %} *)
type formula

(** This exception will be trigerred when a formula will satisfy the removal transformation
    described {% in~\thesisL{Section 7.4.2.5}. %} *)
exception Removal_transformation

(** [create_formula x xi] creates the formula {% $X \neqi \context{\xi}{}$. %}*) 
val create_formula : variable -> recipe -> formula

(** {3 Scanning} *)

(** [for_all_formula f phi] returns [true] iff [f xi_i beta_i] returns [true] for all [i]
    where [phi] is the formula {% $\bigvee_i \xi_i \neqi \beta_i$. %} *)
val for_all_formula : ((context * context) -> bool) -> formula -> bool

(** [exists_formula f phi] returns [true] iff there exists [i] s.t. [f xi_i beta_i] returns [true] 
    where [phi] is the formula {% $\bigvee_i \xi_i \neqi \beta_i$. %} *)
val exists_formula : ((context * context) -> bool) -> formula -> bool

(** [find_and_apply_formula f_test f_apply f_no formula] searches in [formula] an inequation satisfying [f_test]. 
   If such inequation exists then it applies [f_apply] on it else it apply the function [f_no].
   
   Note that since an inequation {% $\xi \neqi \beta$ %} is semantically the same as {% $\beta \neqi \xi$ %}, it is recommanded that
   [f_test xi beta] and [f_test beta xi] are equal. Same for [f_apply]. *)
val find_and_apply_formula : (context -> context -> bool) -> (context -> context -> 'a) -> (unit -> 'a) -> formula -> 'a

(** {3 Modification} *)

(** [apply_substitution_on_formulas theta elt map_elt] first transforms a substitution {% $\theta = \\{X \mapsto \xi\\}$
    into a substitution $\theta' = \\{X \mapsto \gamma\\}$ %} where [gamma] is the result of [context_of_recipe xi].
    Then it applies the substitution [theta'] on the formulas of [elt]. The function
    [map_elt] should map the formulas contained in the element [elt] on which [theta'] should be applied.
    @raise Internal_error if the domain of [theta] is different from a singleton. *)
val apply_substitution_on_formulas : substitution -> 'a -> ('a -> (formula -> formula) -> 'a) -> 'a

(** [simplify_formula phi] returns the formula [phi] simplified as detailed {% in~\thesisL{Section 7.4.2.2}. %}
    @raise Internal_error if [phi] can be simplified into a formula {% $f(\beta_1, \ldots, \beta_n) \neqi g(\beta'_1, \ldots, \beta'_m) \vee \Phi'$ for some $f \neq g$. %}
    @raise Removal_transformation if [phi] can be simplified into a formula of the form {% $\bigvee_i \xi_i \neqi \beta_i$ where for all $i$, $\xi_i \in \Fd^*\cdot \AX$ or $\beta_i \in \Fd^*\cdot \AX$. %} *)
val simplify_formula : formula -> formula

(** [apply_simplify_substitution_on_formulas theta elt map_elt] returns the same as [simplify_formula (apply_substitution_on_formulas theta elt map_elt)]
    but computes it more quickly.*)
val apply_simplify_substitution_on_formulas : substitution -> 'a -> ('a -> (formula -> formula) -> 'a) -> 'a

val display_formula : formula -> string
