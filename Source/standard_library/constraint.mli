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

(** Frame and deducibility constraints *)

(** This module regrous all the functions that manipulate the deduciblity constraints and the frame.
    Hence it corresponds to the elements of the form {% $X, i \deduce u$ and $\xi, j \ded v$ in~\thesisL{Chapter 7,8}. %}
*)

(** {2 Support set} *)

(** Both frame and deducibility constraints are theorically a sets of elements of the form {% $X, i \deduce u$ and $\xi, j \ded v$
    in~\thesis. %} However, both of these element depend of a support, i.e. an integer. Hence to improve the 
    efficiency of our algorithm, the type [support_set] is an optimised set of element parametrised by an integer. *)
type 'a support_set

(** The type [position] corresponds to the specific position of an element in a [support_set].
    It is used to speed-up the access to element of a [support_set]. *)
type position

(** [empty_set] is an empty support set. *)
val empty_set : 'a support_set

(** {3 Modification} *)

(** [add f elt set] add the element [elt] with support [f elt] in the set [set]. [f] should correspond
    to the function that return the support of [elt]. *)
val add : ('a -> int) -> 'a -> 'a support_set -> 'a support_set

(** [add_new_support f set] add the element [f s] in [set] where [s-1] is the support maximal of the
    element in [set]. *)
val add_new_support : (int -> 'a) -> 'a support_set -> 'a support_set 

(** [add_list f elt_list set] add the elements in [elt_list] in the set [set]. [f] should correspond
    to the function that return the support of the elements of [elt_list].
    @raise Internal_error if the application of [f] on the elements of [elt_list] does not return the same value. {% \lowdebug %} *)
val add_list : ('a -> int) -> 'a list -> 'a support_set -> 'a support_set

(** [replace p f set] replace the element in [set] at the position [p] by the elements [f elt]
    if [elt] is the element in [set] at the position [p].
    @raise Internal_error if the position [p] does not correspond to any element in [set].*)
val replace : position -> ('a -> 'a list) -> 'a support_set -> 'a * 'a support_set

(** [replace2 p f set] returns two sets [set1,set2] where [set1] (resp. [set2]) is the set [set] where 
    the element [elt] at the position [p] in [set] is replaced by [elt_l1] (resp. [elt_l2]) with [elt_l1,elt_l2]
    being the result of [f elt].
    @raise Internal_error if the position [p] does not correspond to any element in [set].*)
val replace2 : position -> ('a -> 'a list * 'a list) -> 'a support_set -> 'a * 'a support_set * 'a support_set

(** {3 Scanning} *)

(** [support_range] is a parameter for scanning function.
    It allows more efficient and precise search on the sets. *)
type support_range =
  | SUnique of int  (** [SUnique s] Consider only the elements of support [s]. *)
  | SAll (** Consider all the elements in the set. *)
  | SUntil of int (** [SUntil s] considers only the elements of support inferior or equal to [s]. *)
  | SFrom of int (** [SFrom s] considers only the elements of support superior or equal to [s]. *)
  | SBetween of int * int (** [SBetween s1 s2] considers only the elements of support superior or equal to [s1], and inferior or equal to [s2].*)

(** [search s_range test set] returns [elt,pos] where [elt] is an element in [set] whose support
  satisfies [s_range] and such that [test elt] returns true. [pos] is the position of [elt] in [set].
  @raise Not_found if no element of [set] satisfies the function [test].*)
val search : support_range -> ('a -> bool) -> 'a support_set -> 'a * position

(** [search_and_replace s_range test f set] is an optimisation of
[{ let (elt,pos) = search s_range test set in
elt,pos, replace pos f set}]*)
val search_and_replace : 
  support_range -> 
  ('a -> bool) -> 
  ('a -> 'a list) -> 
  'a support_set -> 
  'a * position * 'a support_set

(** [search_and_replace2 s_range test f set] is an optimisation of
[{ let (elt,pos) = search s_range test set in
let set1,set2 = replace2 pos f set in
elt,pos,set1,set2}]*)
val search_and_replace2 : 
  support_range -> 
  ('a -> bool) -> 
  ('a -> 'a list * 'a list) -> 
  'a support_set -> 
  'a * position * 'a support_set * 'a support_set
  
(** [for_all s_range test set] returns [true] iff for all elements [elt] in [set]
    whose support satisfies [s_range], [test elt] returns [true]. *)
val for_all : support_range -> ('a -> bool) -> 'a support_set -> bool

(** [exists s_range test set] returns [true] iff there exists an element [elt] in [set]
    whose support satisfies [s_range] and such that [test elt] returns [true].*)
val exists : support_range -> ('a -> bool) -> 'a support_set -> bool

(** {3 Access} *)

(** [get pos set] returns the element of [set] at the position [pos].
    @raise Internal_error if [pos] is not a position in [set].*)
val get : position -> 'a support_set -> 'a

(** {3 Iterators} *)
  
(** [iter s_range f set] is [f e1; ...; f en] where apply the function [f] to all elements of [set] whose support satisfies [s_range].
    The order on the element on which [f] is applied is by increasing support first and then
    in the order in which they were added in the set.
    
    Note that the function [replace] modifies the order in which elements are added:
    For example, consider a set [set] of elements with same support such that [elt1], [elt2], [elt3]
    was added in this particular order by call the function [add]. Consider [pos2] the position of [elt2]
    in [set] and the function [g = fun e -> [e;e]].
    We have that [iter SAll f (replace pos2 g set)] is [f elt1; f elt2; f elt2; f elt3].*)
val iter : support_range -> ('a -> unit) -> 'a support_set -> unit
  
(** [map s_range f set] returns the set [set] where the function [f] was applied on all
    the elements of [set] satisfying [s_range]. *)
val map : support_range -> ('a -> 'a) -> 'a support_set -> 'a support_set

val fold_left : support_range -> ('a -> 'b -> 'a) -> 'a -> 'b support_set -> 'a

(** [iter2 s_range f set1 set2] is [f e1 d1; f e2 d2; ...; fen dn] where [set1] (resp. [set2])
    is a set whose elements satisfying [s_range] have the application order [e1;...; en] 
    (resp. [d1;...;dn]). See [Constraint.iter] for more details on the application order.
    @raise Internal_error if [set1] and [set2] do not have the same number of elements  of equal support.*)
val iter2 : support_range -> ('a -> 'a -> unit) -> 'a support_set -> 'a support_set -> unit

(** {3 Display} *)

val display_horizontally : ('a -> string) -> 'a support_set -> string

val display_vertically : ('a -> string) -> string -> 'a support_set -> string

(** {2 Frame} *)

(** {% In~\thesis, a frame is a set $\\{ \xi_1, i_1 \ded u_1; \ldots; \xi_n, i_n \ded u_n\\}$ where 
    $\xi_j \in \T(\F, \AX \cup \Xdeux)$, $\mpath(\xi_j)$ exists and $u_j \in T(\Fc, \N \cup \Xun)$
    for all $j \in \\{1, \ldots, n\\}$. Note that compare to~\thesis, a frame in this implementation is
    extented by the addition of some flags which will represents different notions used 
    later on in the constraint systems. %} *)
    
module Frame :
sig

  (** A frame constraint represents {% in~\thesis~an element of the form $\xi, i \ded_F u$ with  
      $\xi$ a recipe, $i$ a integer, $u$ a constructor term and $F$ a set of flags. %}*)
  type elt

  (** [create frame_constraint xi s m] returns the frame constraint {% $\xi, s \ded_\emptyset m$. %} 
      @raise Internal_error if [m] is not a constructor term. {% \highdebug %} *)   
  val create : Recipe.recipe -> int -> Term.term -> elt

  (** {3 Access} *)    
  
  (** [get_recipe fc] returns the recipe of [fc]. *)
  val get_recipe : elt -> Recipe.recipe
  
  (** [get_suport fc] returns the support of [fc]. *)
  val get_support : elt -> int
  
  (** [get_message fc] returns the message of [fc]. *)
  val get_message : elt -> Term.term
  
  (** {3 Modification} *)
  
  (** [replace_recipe fc rep] returns the frame constraint [fc] with the recipe [rep r]
      where [r] was the recipe of [fc]. *)
  val replace_recipe : elt -> (Recipe.recipe -> Recipe.recipe) -> elt
  
  (** [replace_message fc rep] returns the frame constraint [fc] with the message [rep m]
      where [m] was the message of [fc]. *)
  val replace_message :elt -> (Term.term -> Term.term) -> elt
      
  (** {3 Flags} *)
  
  (** {% In~\thesis, the notion of flag does not exist. However they correspond to 
    other elements or properties of constraint systems. Hence, we will give the semantics
    of each flag when introducting their adding function. For this, we will
    consider a constraint system $\C$, its associated frame $\Phi$ %} and let [fc] be 
    a frame element {% $(\xi, i \ded_F u) \in \Phi$. %} *)
    
  (** [add_noDedSubterm fc f s] adds a flag {% $\textsc{noDedSubterm}(\ffun,s) \in \F$ with $\ffun \in \Fc$.
      It corresponds to the non-deducibility constraint $\ffun(x_1,\ldots, x_n) \neqi u \vee s \not\deduce
      x_1 \vee \ldots \vee s \not\deduce x_n$ where $x_1, \ldots, x_n$ are fresh variables. %}
      @raise Internal_error if a flag {% $\textsc{noUse}$ is alreafy in $\F$. \lowdebug %}
      @raise Internal_error if a flag {% $\textsc{yesDedSubterm}(\gfun,s')$ was already in $F$ for any $\gfun$, $s'$ except when $\gfun = \ffun$ and $s < s'$.%}
      @raise Internal_error if [f] is not a constructor function symbol or if it is a tuple. {% \lowdebug %}
      @raise Internal_error if {% $u \in \Xun$ \lowdebug %}.
  *)
  val add_noDedSubterm : elt -> Term.symbol -> int -> elt
  
  (** [add_YesDedSubterm fc f s] adds a flag {% $\textsc{YesDedSubterm}(\ffun,s) \in \F$. It corresponds to there exists $X_1, \ldots, X_n \in \varsdeux(\C)$
      such that for all $i \in \\{1, \ldots, n\\}$, $\maxparam{X_i\theta}{\C} \leq s$ and 
      \\[
      \context{\ffun(X_1,\ldots,X_n)\theta}{\Phi}\directun{\C} = v
      \\]
      Intuitively, it indicates that $u$ can be constructed in $\C$ by applying $\ffun$ with support inferior or equal to $s$. %}
      @raise Internal_error if a flag {% $\textsc{noDedSubterm}(\ffun,s')$ or $\textsc{noUse}$ was already in $F$ with $s \leq s'$. \lowdebug %}
      @raise Internal_error if [f] is not a constructor function symbol or if it is a tuple. {% \lowdebug %}
      @raise Internal_error if {% $u \in \Xun$ \lowdebug %}.
  *)
  val add_yesDedSubterm : elt -> Term.symbol -> int -> elt
  
  (** [add_noDest fc f s] adds a flag {% $\textsc{NoDest}(\ffun,s) \in \F$ with $f \in \Fd$.
      It corresponds to the non-deducibility constraint $\forall \tilde{x}. u \neqi v_1 \vee s \not\deduce v_2 \vee \ldots vee s \not\deduce v_n$
      where $\ffun(v_1, \ldots, v_n) \rightarrow w$ is a fresh rewrite rule with $\tilde{x} = \vars(v_1)$. %}
      @raise Internal_error if a flag {% $\textsc{yesDest}$ or $\textsc{noUse}$ was already in $F$ %}
      @raise Internal_error if [f] is not a destructor function symbol. {% \lowdebug %}
      @raise Internal_error if [f] is a projection function symbol. {% \highdebug %}
      @raise Internal_error if {% $u \in \Xun$ \lowdebug %}.
  *)
  val add_noDest : elt -> Term.symbol -> int -> elt 
  
  (** [add_yesDest fc] adds a flag {% $\textsc{YesDest} \in F$.
      It corresponds to the fact that there exists $(\zeta, k \ded v) \in \Phi$ such that
      $\mpath(\zeta) = \gfun \cdot \mpath(\xi)$ %} and [Term.link_destruc_construc g f] returns [true]
      where {% $\gfun = \Top{u}$. %}
      @raise Internal_error if a flag {% $\textsc{noUse}$ is already in $\F$. %}
      @raise Internal_error if {% $u \in \Xun$ \lowdebug %}.
  *)
  val add_yesDest : elt -> elt
  
  (** [add_noUse fc] adds a flag {% $\textsc{NoUse} \in F$. 
      It corresponds to the fact that $(\xi, i \ded u) \in \flag(\C)$. %} *)
  val add_noUse : elt -> elt
  
  (** [is_noDedSubterm fc s] returns [true] iff there is a flag {% $\textsc{NoDedSubterm}(\ffun,s') \in F$ with $s \leq s'$
      where $\ffun = \Top{u}$. %} *)
  val is_noDedSubterm : elt -> int -> bool
  
  (** [is_yesDedSubterm fc s] returns [true] iff there is a flag {% $\textsc{YesDedSubterm}(\ffun,s') \in F$ with $s \geq s'$
      where $\ffun = \Top{u}$. %} *)
  val is_yesDedSubterm : elt -> int -> bool
  
  (** [is_noDest fc s] returns [true] iff there is a flag {% $\textsc{NoDest}(\ffun,s') \in F$ with $s \leq s'$
      where $\ffun$ is the corresponding destructor of $\Top{u}$. %} *)
  val is_noDest : elt -> int  -> bool
  
  (** [is_yesDest fc] returns [true] iff there is a flag {% $\textsc{YesDest} \in F$
      where $\ffun = \Top{u}$. %} *)
  val is_yesDest : elt -> bool
  
  (** [is_noUse fc] returns [true] iff there is a flag {% $\textsc{NoUse}$ %}. *)
  val is_noUse : elt -> bool
  
  (** {3 Testing on frame} *)

  (** [is_same_structure frame1 frame2] checks that every couple of frame constraints in [frame1] and [frame2]
      of same application order have:
      {ul
        {li the same recipe }
        {li the same support }
        {li the same set of flags}
      }
  *)
  val is_same_structure : elt support_set -> elt support_set -> bool

  
  (** {3 Display} *)
  
  (** [display fc] display the frame constraint without considering the flags. *)
  val display : elt -> string
  
end


(** {2 Deducibility constraint} *)

(** {% In~\thesis, the deducibility constraints are element of the form $X, i \deduce u$ where 
    $X \in \Xdeux$, $i \in \mathbb{N}$ and $u \in \T(\Fc, \N \cup \Xun)$.
    Note that compare to~\thesis, a deducibility constraints in this implementation is
    extented by the addition of some flags which will represents different notions used 
    later on in the constraint systems. %} *)

module Deducibility :
sig
  type elt
  
  (** [create v s t] creates a deducibility constraint with the variable [v], the support [s] and
      the term [t].
      @raise Internal_error if [t] is not a constructor term. {% \highdebug %}
      @raise Internal_error if [s] is different from the support of [v]. *)
  val create : Recipe.variable -> int -> Term.term -> elt
  
  (** {3 Access} *)    
  
  (** [get_recipe_variable dc] returns the recipe variable of [dc]. *)
  val get_recipe_variable : elt -> Recipe.variable
  
  (** [get_support dc] returns the support of [dc]. *)
  val get_support : elt -> int
  
  (** [get_message dc] returns the message of [dc]. *)
  val get_message : elt -> Term.term
  
  (** {3 Modification} *)
  
  (** [replace_message cc rep] returns the deducibility constraint [dc] with the message [rep m]
      where [m] was the message of [dc]. *)
  val replace_message : elt -> (Term.term -> Term.term) -> elt
  
  (** {3 Flags} *)
  
  (** {% Similarly to the module [Frame], the flags in deducibility constraint correspond to 
      other elements or properties of constraint systems. Hence, we will give the semantics
      of each flag when introducting their adding function. For this, we will
      consider a constraint system $\C$, its associated deducibility constraint set $\De$ %} and let [dc] be 
      a deducibility constraint {% $(X, i \deduce_F u) \in \De$. %}*)
  
  (** [add_noCons dc f] adds the flag {% $\textsc{NoCons}(\ffun) \in F$. It corresponds to the
      inequation $\Top(X) \neqi \ffun$. %}
      @raise Internal_error if the flag was already added. {% \lowdebug %}
  *)
  val add_noCons : elt -> Term.symbol -> elt
  
  (** [add_noAxiom dc p] add the flag {% $\textsc{NoAxiom}(p) \in \F$. 
      It corresponds to the inequation $X \neqi \xi$ where $(\xi, j \ded v)$ is the 
      frame constraint in $\Phi(\C)$ at the position %} [p].
      Note: the flags {% $\textsc{NoAxiom} %} must be added by the rule {% $\textsc{Axiom}$ %} for all 
      cases except when a {% $\textsc{NoUse} %} is detected.
      @raise Internal_error if the flag was already added. {% \lowdebug %}
  *)
  val add_noAxiom : elt -> position -> elt
  
  (** [compare_noCons dc1 dc2] compare the flags {% \textsc{NoCons} %} in [dc1] and [dc2]. 
      It returns a pair of set of function symbols {% $(S_1,S_2)$ %} where:
      {ul 
        {li {% for all $\ffun \in S_1$, $\textsc{NoCons}(\ffun) \in \F_2$ but $\textsc{NoCons}(\ffun) \not\in \F_1$. %} }
        {li {% for all $\ffun \in S_2$, $\textsc{NoCons}(\ffun) \in \F_1$ but $\textsc{NoCons}(\ffun) \not\in \F_2$.%} } 
      }
  *)
  val compare_noCons : elt -> elt -> Term.symbol list * Term.symbol list
  
  (** [compare_noAxiom c1 c2 s] compare the flags {% \textsc{NoAxiom} %} in [dc1] and [dc2]. 
      It returns a pair of set of position {% $(P_1,P_2)$ %} where:
      {ul
        {li {% for all $p \in P_1$ of support $s$, $\textsc{NoAxiom}(p) \in F_2$ but $\textsc{NoAxiom}(p) \not\in F_1$. %} }
        {li {% for all $p \in P_2$ of support $s$, $\textsc{NoAxiom}(p) \in F_1$ but $\textsc{NoAxiom}(p) \not\in F_2$. %} }
      }
  *)
  val compare_noAxiom :elt -> elt -> int -> position list * position list
  
  (** [fold_left_frame_free_of_noAxiom dc f acc frame] is similar to [fold_left (SUntil s) f acc frame]
      but [f] is only applied to the element of position [pos] of [frame] such that 
      the flag [NoAxiom pos] is not in [dc]. Moreover, [s] is the support of [dc] *)
  val fold_left_frame_free_of_noAxiom : elt -> ('a -> Frame.elt -> 'a) -> 'a -> Frame.elt support_set -> 'a
  
  (** {3 Scanning} *)
  
  (** [is_all_noCons dc] returns [true] iff the flags [NoCons f] is in [dc] for all constructors [f]. *) 
  val is_all_noCons : elt -> bool
  
  (** [is_same_structure dc_set1 dc_set2] checks that every couple of deducibility constraints in [dc_set1] and [dc_set2]
      of same application order have:
      {ul
        {li the same variable }
        {li the same support }
        {li the same set of flags}
      }
  *)
  val is_same_structure : elt support_set -> elt support_set -> bool
  
  val is_noCons : elt -> Term.symbol -> bool
  
  (* [is_unsatisfiable frame dc] returns [true] iff the deducibility constraint [dc] is unsatisfiable
     under the frame [frame]. Note that the interest of having flags in the frame and deducibility constraints sets
     is that we do not need the full definition of a constraint system to ensure the unsatisfiability of 
     a deducibility constraint. {% It corresponds to the rule (Nnosol) in~\thesisL{Figure 7.4}. %}
     @raise Internal_error if the rule {% $\Dest(\xi, \ell \rightarrow r, i)$ is not useless with $i$ being the support of %} [dc]*)
  val is_unsatisfiable : Frame.elt support_set -> elt -> bool
  
  (** {3 Display} *)
  
  val display : elt -> string
end

