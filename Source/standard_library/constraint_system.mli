(** Operations on (matrices of) constraint systems *)

(** This module regrous all the functions that manipulate the constraint systems and the
    matrices of constraint system. {% In~\thesis, there are several definitions of constraint systems
    but we are only interested in the constraint system of~\thesisL{Chapter 7}. %}
*)

(** {2 Constraint system} *)

(** [constraint_system] corresponds {% to~\thesisL{Definition 7.6} %}. Moreover, it will contain
    additional information used in the algorithm such as association table {% (see~\thesisL{Section 7.4.2.2}). %} *)
type constraint_system

(** [empty] is the constraint system that accept any solution. It does not contain any
    deducibility constraint, nor frame constraint, nor equation, nor inequation. *)
val empty : constraint_system

(** [bottom] is the constraint system with no solution. It corresponds to {% $\bot$~in \thesis. %} *)
val bottom : constraint_system

(** {3 Iterators} *)

val map_message_inequation : (Term.formula -> Term.formula) -> constraint_system -> constraint_system

(** {3 Modification functions} *)

(** [add_message_equation csys t1 t2] returns the constraint system [csys] with the added equation {% $t_1 \eqi t_2$. %} *)
val add_message_equation : constraint_system -> Term.term -> Term.term -> constraint_system

(** [add_message_formula csys phi] returns the constraint system [csys] with the added message formula [phi] *)
val add_message_formula : constraint_system -> Term.formula -> constraint_system

(** [add_new_deducibility_constraint csys X t] returns the constraint system [csys] with the added deducibility constraint
    {% $X, i \deduce t$ where $i$ %} is the maximal support of [csys].
    @raise Internal_error if [csys] is the bottom constraint system.
    @raise Internal_error if [t] is not a constructor term. {% \highdebug %}
    @raise Internal_error if the support associated to [X] is not equal to the maximal support of [csys]. *)
val add_new_deducibility_constraint : constraint_system -> Recipe.variable -> Term.term -> constraint_system

val add_deducibility_constraint : constraint_system -> Constraint.Deducibility.elt list -> constraint_system

(** [add_new_dependency_constraint csys dep] add the constraint [dep] to the dependency
constraints of [csys]. *)
val add_new_dependency_constraint : constraint_system -> Recipe.recipe list -> Recipe.axiom list -> constraint_system

(** [add_new_axiom csys t] returns the constraint system [csys] with the frame {% $\Phi \cup \\{ \ax_i, i \ded t\\}$ %}
    where {% $\Phi$ %} is the frame of [csys] and $i-1$ is the maximal support of {% $\Phi$ %}.
    @raise Internal_error if [csys] is the bottom constraint system. *)
val add_new_axiom : constraint_system -> Term.term -> constraint_system

val add_frame_constraint : constraint_system -> Constraint.Frame.elt list -> constraint_system
    
(** [frame_replace c p f] replace the element in the frame of [c] at the position [p] by the elements [f elt]
    if [elt] is the element in the frame of [c] at the position [p].
    @raise Internal_error if the position [p] does not correspond to any element in the frame of [c].*)  
val frame_replace :
  constraint_system -> 
  Constraint.position ->
  (Constraint.Frame.elt -> Constraint.Frame.elt list) ->
  Constraint.Frame.elt * constraint_system
  
(** [frame_replace2 c p f] returns two constraint systems [c1,c2] where [c1] (resp. [c2]) is the constraint system [c] where 
    the element [elt] at the position [p] in the frame of [c] is replaced by [elt_l1] (resp. [elt_l2]) with [elt_l1,elt_l2]
    being the result of [f elt].
    @raise Internal_error if the position [p] does not correspond to any element in the frame of [c].*)
val frame_replace2 :
  constraint_system -> 
  Constraint.position ->
  (Constraint.Frame.elt -> Constraint.Frame.elt list * Constraint.Frame.elt list) ->
  Constraint.Frame.elt * constraint_system * constraint_system
  
(** [frame_search_and_replace c s_range test f] is an optimisation of
[{ let (elt,pos) = Constraint.search s_range test (get_frame c) in
elt,pos, frame_replace c pos f}]*)
val frame_search_and_replace : 
  constraint_system -> 
  Constraint.support_range -> 
  (Constraint.Frame.elt -> bool) -> 
  (Constraint.Frame.elt -> Constraint.Frame.elt list) ->
  Constraint.Frame.elt * Constraint.position * constraint_system
  
(** [frame_search_and_replace2 c s_range test f] is an optimisation of
[{ let (elt,pos) = Constraint.search s_range test (get_frame c) in
let set1,set2 = frame_replace2 c pos f in
elt,pos,set1,set2}]*)  
val frame_search_and_replace2 : 
  constraint_system -> Constraint.support_range ->
  (Constraint.Frame.elt -> bool) ->
  (Constraint.Frame.elt -> Constraint.Frame.elt list * Constraint.Frame.elt list) ->
  Constraint.Frame.elt * Constraint.position * constraint_system * constraint_system  

(** {3 Access functions} *)

(** [get_deducibility_constraint_set csys] returns the set of deducibility constraints of [csys].
    @raise Internal_error if [csys] is the bottom constraint system.*)
val get_deducibility_constraint_set : constraint_system -> Constraint.Deducibility.elt Constraint.support_set

(** [get_frame csys] returns the frame of [csys].
    @raise Internal_error if [csys] is the bottom constraint system.*)
val get_frame : constraint_system -> Constraint.Frame.elt Constraint.support_set

(** [get_message_equations csys] returns the list [[(u_1,v_1);...;(u_n,v_n)]] where {% $\bigwedge_{i =1}^n u_i \eqi v_i$ %} is the conjunction
    of equations between constructor terms in [csys].
    @raise Internal_error if [csys] is the bottom constraint system.*)
val get_message_equations : constraint_system -> (Term.term * Term.term) list

(** [get_recipe_equations csys] returns the list [[(xi_1,zeta_1);...;(xi_n,zeta_n)]] where {% $\bigwedge_{i =1}^n \xi_i \eqi \zeta_i$ %} is the conjunction
    of equations between recipes in [csys]. 
    @raise Internal_error if [csys] is the bottom constraint system.*)
val get_recipe_equations : constraint_system -> (Recipe.recipe * Recipe.recipe) list

(** [get_dependency_constraints csys] returns the dependency constraints. *)
val get_dependency_constraints : constraint_system -> (Recipe.recipe list * Recipe.axiom list) list

(** [get_maximal_support csys] returns maximal support of the frame of [csys].
    @raise Internal_error if [csys] is the bottom constraint system.*)
val get_maximal_support : constraint_system -> int

(** {3 Testing functions} *)

val is_semi_solved_form : constraint_system -> bool

val set_semi_solved_form : constraint_system -> constraint_system

val unset_semi_solved_form : constraint_system -> constraint_system


val is_no_universal_variable : constraint_system -> bool

val set_no_universal_variable : constraint_system -> constraint_system

val unset_no_universal_variable : constraint_system -> constraint_system

(** [is_bottom c] returns [true] iff [c] is the constraint system {% $\bot$. %} *)
val is_bottom : constraint_system -> bool

(** [check_same_structure c1 c2] does nothing if [c1] and [c2] have same structure else it raises 
    the exception [Internal_error]. The definition of structure is given {% in~\thesisL{Section 7.1.2}. %} *)
val check_same_structure : constraint_system -> constraint_system -> unit

(** [check_same_shape c1 c2] does nothing if [c1] and [c2] have same shape else it raises 
    the exception [Internal_error]. The definition of shape is given {% in~\thesisL{Definition 7.11}. %} *)
val check_same_shape : constraint_system -> constraint_system -> unit

val display : constraint_system -> string

val display_dependency_constraints : constraint_system -> string

val is_unsatisfiable : constraint_system -> bool

(** {2 Functionnalities of Phase 1} *)

(** In the strategy on the rules described {% in~\thesisL{Section 7.4}, %} there are two different phases
    of rule application. Hence this section describes the optimised functions used in Phase 1
    of the strategy. Due to the lack of invariant during this phase, these functions are 
    quite general. *)

module Phase_1 :
sig

  (** [activate_phase csys] returns the constraint system [csys] optimised for Phase 1 of the strategy. *)
  val activate_phase : constraint_system -> constraint_system
  
  (** {3 Modifications} *)
  
  (** [deducibility_replace c p f] replace the deducibility constraint of [c] at the position [p] by the deducibility constraints [f dc]
      if [dc] is the deducibility constraint of [c] at the position [p].
      @raise Internal_error if the position [p] does not correspond to any deducibility constraint in [c].*)  
  val deducibility_replace :
    constraint_system -> 
    Constraint.position ->
    (Constraint.Deducibility.elt -> Constraint.Deducibility.elt list) ->
    Constraint.Deducibility.elt * constraint_system
    
  (** [deducibility_replace2 c p f] returns two constraint systems [c1,c2] where [c1] (resp. [c2]) is the constraint system [c] where 
      the deducibility constraint [dc] at the position [p] in [c] is replaced by [dc_l1] (resp. [dc_l2]) with [dc_l1,dc_l2]
      being the result of [f dc].
      @raise Internal_error if the position [p] does not correspond to any deducibility constraint in [c].*)
  val deducibility_replace2 :
    constraint_system -> 
    Constraint.position ->
    (Constraint.Deducibility.elt -> Constraint.Deducibility.elt list * Constraint.Deducibility.elt list) ->
    Constraint.Deducibility.elt * constraint_system * constraint_system
  
  (** [deducibility_search_and_replace c s_range test f] is an optimisation of
[{ let (elt,pos) = Constraint.search s_range test (get_deducibility_constraint_set c) in
elt,pos, deducibility_replace c pos f}]*)
  val deducibility_search_and_replace : 
    constraint_system -> 
    Constraint.support_range ->
    (Constraint.Deducibility.elt -> bool) ->
    (Constraint.Deducibility.elt -> Constraint.Deducibility.elt list) ->
    Constraint.Deducibility.elt * Constraint.position * constraint_system

(** [deducibility_search_and_replace2 c s_range test f] is an optimisation of
[{ let (elt,pos) = Constraint.search s_range test (get_deducibility_constraint_set c) in
let set1,set2 = deducibility_replace2 c pos f in
elt,pos,set1,set2}]*)  
  val deducibility_search_and_replace2 : 
    constraint_system -> 
    Constraint.support_range ->
    (Constraint.Deducibility.elt -> bool) ->
    (Constraint.Deducibility.elt -> Constraint.Deducibility.elt list * Constraint.Deducibility.elt list) ->
    Constraint.Deducibility.elt * Constraint.position * constraint_system * constraint_system
   
   (** {3 Substitution} *)  
    
  (** [unify_and_apply_message_equations csys eq_l] returns the normalised constraint system [csys] on which 
      the most general unifier of [eq_l] was applied.
      @raise Term.Not_unifiable if [eq_l] is no unifiable. *)
  val unify_and_apply_message_equations : constraint_system -> (Term.term * Term.term) list -> constraint_system

  (** [apply_message_equations csys subst] returns the normalised constraint system [csys] on which 
      [subst] was applied. *)
  val apply_message_substitution : constraint_system -> Term.substitution -> constraint_system

  (** [apply_recipe_substitution csys subst] returns the normalised constraint system [csys] on which 
      [subst] was applied. 
      @raise Internal_error if the domain of [subst] intersects with the left hand side variables of [csys]. {% \highdebug %} *)
  val apply_recipe_substitution : constraint_system -> Recipe.substitution -> constraint_system
  
  (** [normalise csys] returns the constraint system [csys] normalised. It may contain destructors function symbol in
      inequations and equations. {% This normalisation corresponds to the transformation induced by~\thesisL{Lemma 6.10}. %} *)
  val normalise : constraint_system -> constraint_system
  
end

(** {2 Functionnalities of Phase 2} *)

(** As mention in the previous section, there are two different phases
    of rule application described in the strategy {% (see~\thesisL{Section 7.4}). %} This section describes the 
    optimised functions used in Phase 2 of the strategy.
    These functions will benefit from the fact that the right hand term of constraint system are variables.
    On the other hand, they consider the association tables in the constraint system.*)

module Phase_2 :
sig
  
  (** [activate_phase csys] returns the constraint system [csys] optimised for Phase 2 of the strategy. *)
  val activate_phase : constraint_system -> constraint_system
  
  val add_message_inequation : 
    constraint_system -> 
    Term.variable -> Term.term -> 
    Recipe.variable -> Recipe.recipe -> 
    constraint_system
  
  (** {3 Modifications} *)
  
  (** See [Phase_1.deducibility_replace]. *)
  val deducibility_replace :
    constraint_system -> 
    Constraint.position ->
    (Constraint.Deducibility.elt -> Constraint.Deducibility.elt list) ->
    Constraint.Deducibility.elt * constraint_system
    
  (** See [Phase_1.deducibility_replace2]. *)
  val deducibility_replace2 :
    constraint_system -> 
    Constraint.position ->
    (Constraint.Deducibility.elt -> Constraint.Deducibility.elt list * Constraint.Deducibility.elt list) ->
    Constraint.Deducibility.elt * constraint_system * constraint_system
  
  (** See [Phase_1.deducibility_search_and_replace]. *)
  val deducibility_search_and_replace : 
    constraint_system -> 
    Constraint.support_range ->
    (Constraint.Deducibility.elt -> bool) ->
    (Constraint.Deducibility.elt -> Constraint.Deducibility.elt list) ->
    Constraint.Deducibility.elt * Constraint.position * constraint_system
  
  (** See [Phase_1.deducibility_search_and_replace2]. *)
  val deducibility_search_and_replace2 : 
    constraint_system -> 
    Constraint.support_range ->
    (Constraint.Deducibility.elt -> bool) ->
    (Constraint.Deducibility.elt -> Constraint.Deducibility.elt list * Constraint.Deducibility.elt list) ->
    Constraint.Deducibility.elt * Constraint.position * constraint_system * constraint_system
  
  (** {3 Substitution} *)
  
  (** See [Phase_1.unify_and_apply_message_equations]. *)
  val unify_and_apply_message_equations : constraint_system -> (Term.term * Term.term) list -> constraint_system

  (** See [Phase_1.apply_message_substitution]. *)
  val apply_message_substitution : constraint_system -> Term.substitution -> constraint_system

  (** See [Phase_1.apply_recipe_substitution]. *)
  val apply_recipe_substitution : constraint_system -> Recipe.substitution -> constraint_system
  
  (** {3 Access functions} *)

  (** [term_of_recipe c xi] returns the term {% $\xi\directun{\C}$. %}
      @raise Internal_error if {% $\xi \not\in \T(\Fc, \Xdeux)$ %}
      @raise Not_found if {% $\varsdeux(\xi) \smallsetminus \varsdeux(\De(\C)) \neq \emptyset$. %} *)
  val term_of_recipe : constraint_system -> Recipe.recipe -> Term.term

  (** [recipe_of_term c t] returns the recipe {% $\xi$ such that $\xi\directun{\C} = t$. %}
      @raise Internal_error if {% $t \not\in \T(\Fc, \Xun)$ %}
      @raise Not_found if {% $\varsun(\xi) \smallsetminus \varsun(\De(\C)) \neq \emptyset$. %} *)
  val recipe_of_term : constraint_system -> Term.term -> Recipe.recipe

  (** [get_max_param_context c xi] returns the interger {% $\maxparamC{\C}{\context{\xi}{\C}}$. %} *)
  val get_max_param_context : constraint_system -> Recipe.recipe -> int

  (** [get_max_param_context_from_term c t] returns the same result as
      [get_max_param_context c (recipe_of_term c t)] but is more efficient. *)
  val get_max_param_context_from_term : constraint_system -> Term.term -> int
  
  (** {3 Formula inequation functions} *)
  
  val map_message_inequations : 
    (Term.formula -> Recipe.formula option -> Term.formula * Recipe.formula option) -> 
    constraint_system -> constraint_system
    
  val fold_left_message_inequation : 
    ('a -> Term.formula -> Recipe.formula option -> 'a) -> 'a -> constraint_system -> 'a
end

(** {2 Row matrix of constraint system} *)

(** The types [vector] and [matrix] corresponds to the vectors and matrices of constraint
    systems used {% in~\thesisL{Chapter 7-8}. %} *)

type row_matrix

module Row :
sig
  exception All_bottom
 
  (** [Row.create_row_matrix s csys_l] creates a row matrix of constraint system of size [s] where 
    the element are the constraint systems in [csys_l].
    @raise Internal_error if the constraint systems in [csys_l] do not have the same structure. {% \highdebug %}
    @raise Internal_error if [s] is different from the number of element in [csys_l]
    @raise Internal_error if the elements of [csys_l] do not have the same maximal support.
  *)
  val create : int -> constraint_system list -> row_matrix
  
  val get : row_matrix -> int -> constraint_system
  
  (** [Row.get_number_column rm] returns the number of column of [rm]. *)
  val get_number_column : row_matrix -> int
  
  (** [get_maximal_support rm] returns the maximal support of the constraint systems in [rm]. *)
  val get_maximal_support : row_matrix -> int

  val iter : (constraint_system -> unit) -> row_matrix -> unit
  
  val map : (constraint_system -> constraint_system) -> row_matrix -> row_matrix
  
  val map2 : ('a -> constraint_system -> constraint_system) -> 'a list -> row_matrix -> row_matrix
  
  val fold_right : (constraint_system -> 'a -> 'a) -> row_matrix -> 'a -> 'a
  
  val fold_left : ('a -> constraint_system -> 'a) -> 'a -> row_matrix -> 'a
  
  (** [check_structure rm] does nothing if [rm] have is well structured else it raises 
      the exception [Internal_error]. The definition of well structured row matrix is given {% in~\thesisL{Section 7.3.2.1}. %} *)
  val check_structure : row_matrix -> unit
  
end

(** {2 Matrix of constraint systems} *)

type matrix

module Matrix :
sig

  val empty : matrix

  (** [matrix_of_row_matrix rm] returns the row matrix [rm] considered as a matrix with one line. *) 
  val matrix_of_row_matrix : row_matrix -> matrix

  val add_row : matrix -> row_matrix -> matrix
  
  (** {3 Access} *)

  (** [get_number_column m] returns the number of column of [m]. *)
  val get_number_column : matrix -> int

  (** [get_number_line m] returns the number of line of [m]. *)
  val get_number_line : matrix -> int

  (** [get_maximal_support m] returns the maximal support of the constraint systems in [m]. *)
  val get_maximal_support : matrix -> int

  (** {3 Iterators} *)

  (** If [m] is the matrix {% $[V_1;\ldots;V_n]$ where the $V_i$ are row matrices, %} then 
      [replace_row m f] returns the matrix {% $[V^1_1; \ldots; V^{k_1}_1; V^1_2; \ldots; V^{k_n}_n]$
      where for all $i \in \{1, \ldots, n\}$, %} the application of [f] on {% $V_i$ is the
      list of row matrices $V^1_i, \ldots, V^{k_i}_i$. %}
      @raise Internal_error if the maximal support of the number of column of the row matrices produced by [f] do not match. *)
  val replace_row : (row_matrix -> row_matrix list) -> matrix ->  matrix
  
  (** [fold_left_column j f acc m] is [f (.. f (f acc c1) c2 ..) cn] where [[c1;...;cn]] is the vector 
      of constraint systems corresponding to the [j]th column of [m]. *)
  val fold_left_on_column : int -> ('a -> constraint_system -> 'a) -> 'a -> matrix -> 'a

  (** [fold_left_row j f acc m] is [f (.. f (f acc c1) c2 ..) cn] where [[c1;...;cn]] is the vector 
      of constraint systems corresponding to the [j]th line of [m]. *)
  val fold_left_on_row : int -> ('a -> constraint_system -> 'a) -> 'a -> matrix -> 'a  
 
  val fold_left_row : ('a -> row_matrix -> 'a) -> 'a -> matrix -> 'a
  
  val fold_right_row : (row_matrix -> 'a -> 'a) -> matrix -> 'a -> 'a
  
  (** [iter f matrix] is [f c_1_1; f c_1_2; ...; f c_1_m; f c_2_1; ...; f c_n_m] where [matrix] is the matrix
      {% \\[
      \left[
      \begin{array}{ccc}
      c_{1,1} & \cdots & c_{1,m} \\
      \vdots & \ddots & \vdots\\
      c_{n,1} & \cdots & c_{n,m} \\
      \end{array}
      \right]
      \\]  %}*)
  val iter : (constraint_system -> unit) -> matrix -> unit
  
  val iter_row : (row_matrix -> unit) -> matrix -> unit
  
  val map : (constraint_system -> constraint_system) -> matrix -> matrix

  val map_on_column : int -> (constraint_system -> constraint_system) -> matrix -> matrix
  
  (** {3 Matrix searching} *)

  (** [find_in_row i f_test matrix] searches the first constraint system in the line [i] of [matrix] that satisfies [f_test].
      @raise Not_found if no such constraint system exists. *)
  val find_in_row : int -> (constraint_system -> bool) -> matrix -> constraint_system * int

  (** [find_in_col j f_test matrix] searches the first constraint system in the column [j] of [matrix] that satisfies [f_test].
    @raise Not_found if no such constraint system exists.*)
  val find_in_col : int -> (constraint_system -> bool) -> matrix -> constraint_system * int
 
  (** [find_in_row_between_col_index i j j' f_test matrix] searches the first constraint system in line [i] of [matrix] that satisfies [f_test]
      and whose column index is between [j] and [j']. 
      @raise Not_found if no such constraint system exists.
      @raise Internal_error if the column indexes are not correct. {% \lowdebug %} *)
  val find_in_row_between_col_index : int -> int -> int -> (constraint_system -> bool) -> matrix -> constraint_system * int
  
  (** [find_in_col_between_row_index j i i' f_test matrix] searches the first constraint system in column [j] of [matrix] that satisfies [f_test]
      and whose line index is between [i] and [i'].
      @raise Not_found if no such constraint system exists.*)
  val find_in_col_between_row_index : int -> int -> int -> (constraint_system -> bool) -> matrix -> constraint_system * int
 
  (** {3 Matrix scanning} *)

  (** [exists_in_row i f_test matrix] retrurns true iff there exists a constraint system in the line [i] of [matrix] that satisfies [f_test].
      *)
  val exists_in_row : int -> (constraint_system -> bool) -> matrix -> bool
  
  (** [exists_in_row i j j' f_test matrix] retrurns true iff there exists a constraint system in the line [i] of [matrix] that satisfies [f_test]
    and whose column index is between [j] and [j'].*)
  val exists_in_row_between_col_index : int -> int -> int -> (constraint_system -> bool) -> matrix -> bool
  
  (** [exists_in_col j f_test matrix] retrurns true iff there exists a constraint system in the column [j] of [matrix] that satisfies [f_test].*)
  val exists_in_col : int -> (constraint_system -> bool) ->  matrix -> bool
  
  (** [exists_in_col j i i' f_test matrix] retrurns true iff there exists a constraint system in the column [j] of [matrix] that satisfies [f_test]
    and whose line index is between [i] and [i'].*)
  val exists_in_col_between_row_index : int -> int -> int -> (constraint_system -> bool) -> matrix -> bool

  (** [is_empty m] returns [true] iff and only [m] is the empty matrix. *)
  val is_empty : matrix -> bool


  (** [check_structure m] does nothing if [m] have is well structured else it raises 
    the exception [Internal_error]. The definition of well structured matrix is given {% in~\thesisL{Section 7.3.2.1}. %} *)
  val check_structure : matrix -> unit
  
  val display : matrix -> string
  
  val normalise : matrix -> matrix
end


(** {2 Rule applications} *)

(** The exception [Not_applicable] is launched when a rule cannot be applied on a row matrix
    usually due to a condition of the structure of the constraint systems in the row. *)
exception Not_applicable

(** The following functions describe the mechanism for applying a rule on matrices of
    constraint system. Each of these functions have as arguments at least the two following functions:
    {ul
      {li [search : constraint_system -> 'a * constraint_system * constraint_system]}
      {li [apply : 'a -> constraint_system -> constraint_system * constraint_system]}
    }
    Typically, applying a rule on a constraint system depend on parameter that can depend themselves on
    elements of the frame, deducibility constraints, equations, ... The function [search] searches
    for the correspondances between the paramaters of the rule and the constraint system, then it applies 
    the rule on the constraint system hence producing two new constraint systems. However, since a rule will 
    always be applied on row matrices that contains constraint systems of same structure, [search] also
    returns enough informations for the function [apply] to apply the rules on a constraint system without 
    having to search again the correspondance between parameter and the constrain system.
    
    {% \medskip %}
*)

(** [apply_rule_on_row_matrix search apply r] apply the rule on the row matrix [r]. 
    It returns a pair of row matrix option [(r_left,r_right)] where [r_left] (resp. [r_right])
    is [None] if the application of the rule produces an unsatisfiable left (resp. right) row matrix, 
    i.e. a row matrice with only {% $\bot$ %} as constraint systems.
    {% See~\thesisL{Definition 7.10} for more detail on the application of a rule on a row matrix. %}
    @raise Internal_error if the constraint systems produced by [search] or [apply] 
    do not have the same maximal supports as those in [r].
*)
val apply_rule_on_row_matrix : 
  (constraint_system -> 'a * constraint_system * constraint_system) ->
  ('a -> constraint_system -> constraint_system * constraint_system) ->
  row_matrix ->
  (row_matrix option) * (row_matrix option)

(** [apply_external_rule search apply m] apply an external rule on the matrix [m]. 
    {% See~\thesisL{Section 7.3.2.2} for more detail on the application of an external rule on a matrix. %}
    @raise Internal_error if the constraint systems produced by [search] or [apply] 
    do not have the same maximal supports as those in [m].
*)  
val apply_external_rule :
  (constraint_system -> 'a * constraint_system * constraint_system) ->
  ('a -> constraint_system -> constraint_system * constraint_system) ->
  matrix ->
  matrix * matrix

(** [apply_internal_rule search apply i m] apply an internal rule on the [i]th line of matrix [m]. 
    {% See~\thesisL{Section 7.3.2.2} for more detail on the application of an internal rule on a matrix. %}
    @raise Internal_error if the constraint systems produced by [search] or [apply] 
    do not have the same maximal supports as those in [m].
    @raise Internal_error if [i] is not the index of a line of [m].
*)   
val apply_internal_rule : 
  (constraint_system -> 'a * constraint_system * constraint_system) ->
  ('a -> constraint_system -> constraint_system * constraint_system) ->
  int -> 
  matrix ->
  matrix
 
(** [apply_internal_rule_full_column search apply m] apply an internal rule on each line line of matrix [m] hence
    returning a matrix with twice the number of line as [m] (when counting the line with only bottom constraint system). 
    It will be used to apply rule {% \Dest and \Eqlr %}.
    {% See~\thesisL{Section 7.4.1.1} for more detail on the application of these rules. %}
    @raise Internal_error if the constraint systems produced by [search] or [apply] 
    do not have the same maximal supports as those in [m].
*)  
val apply_internal_rule_full_column : 
  (constraint_system -> 'a * constraint_system * constraint_system) ->
  ('a -> constraint_system -> constraint_system * constraint_system) ->
  matrix ->
  matrix
