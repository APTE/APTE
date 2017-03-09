(***********************************
***          Support Set         ***
************************************)

type 'a support_set = (int * ('a list)) list

type position = int * int

let empty_set = []

(********* Modification  *********)

let add f_support elt set =
  let support = f_support elt in

  let rec add_r = function
    | [] -> [(support,[elt])]
    | (s,l)::q when s = support -> (s,l@[elt])::q
    | (s,l)::q when s > support -> (support,[elt])::(s,l)::q
    | t::q -> t::(add_r q)
  in

  add_r set

let add_list f_support elt_list set =
  if elt_list = []
  then set
  else
    let support = (f_support (List.hd elt_list)) in

    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if List.exists (fun elt -> (f_support elt) <> support) (List.tl elt_list)
      then Debug.internal_error "[constraint.ml >> Frame.add_list] The element of the list does not have all the same support";
    );
    (***[END DEBUG]***)

    let rec add_r = function
      | [] -> [(support,elt_list)]
      | (s,l)::q when s = support -> (s,l@elt_list)::q
      | (s,l)::q when s > support -> (support,elt_list)::(s,l)::q
      | t::q -> t::(add_r q)
    in

  add_r set

let add_new_support f_elt set =
  let rec rec_add i = function
    | [] -> [i,[f_elt i]]
    | (s,l)::q -> (s,l)::(rec_add (s+1) q)
  in

  rec_add 1 set

let replace (support,pos_elt) f_replace set =
  let result = ref None in

  let rec replace_sub pos = function
    | [] -> Debug.internal_error "[constraint.ml >> replace] The position given as argument is not a position of the set given as argument (1)"
    | elt::q when pos = 1 ->
        result := Some(elt);
        (f_replace elt)@q
    | elt::q -> elt::(replace_sub (pos-1) q)
  in

  let rec replace_support = function
    | [] -> Debug.internal_error "[constraint.ml >> replace] The position given as argument is not a position of the set given as argument (2)"
    | (s,elt_list)::q when s = support ->
        let elt_l' = replace_sub pos_elt elt_list in
        if elt_l' = []
        then q
        else (s,elt_l')::q
    | t::q -> t::(replace_support q)
  in

  let set' = replace_support set in

  match !result with
    | Some(r) -> r,set'
    | _ -> Debug.internal_error "[constraint.ml >> replace] Unexpected case"

let replace2 (support,pos_elt) f_replace set =
  let result = ref None in

  let rec replace_sub pos = function
    | [] -> Debug.internal_error "[constraint.ml >> replace2] The position given as argument is not a position of the set given as argument (1)"
    | elt::q when pos = 1 ->
        result := Some(elt);
        let elt_list1, elt_list2 = f_replace elt in
        elt_list1@q, elt_list2@q
    | elt::q ->
        let elt_list1, elt_list2 = replace_sub (pos-1) q in
        elt::elt_list1, elt::elt_list2
  in

  let rec replace_support = function
    | [] -> Debug.internal_error "[constraint.ml >> replace2] The position given as argument is not a position of the set given as argument (2)"
    | (s,elt_list)::q when s = support ->
        let elt_list1, elt_list2 = replace_sub pos_elt elt_list in
        let set1 = if elt_list1 = [] then q else (s,elt_list1)::q
        and set2 = if elt_list2 = [] then q else (s,elt_list2)::q in

        set1,set2
    | t::q ->
        let set1,set2 = replace_support q in
        t::set1, t::set2
  in

  let set1',set2' = replace_support set in

  match !result with
    | Some(r) -> r,set1',set2'
    | _ -> Debug.internal_error "[constraint.ml >> replace] Unexpected case"

(********* Scanning  *********)

type support_range =
  | SUnique of int
  | SAll
  | SUntil of int
  | SFrom of int
  | SBetween of int * int

let generate_test_support_range = function
  | SUnique s_eq ->
      (fun s -> s > s_eq), (fun s -> s >= s_eq)
  | SAll ->
      (fun _ -> false), (fun _ -> true)
  | SUntil s_sup ->
      (fun s -> s > s_sup), (fun _ -> true)
  | SFrom s_inf ->
      (fun _ -> false), (fun s -> s >= s_inf)
  | SBetween (s_inf,s_sup) ->
      (fun s -> s > s_sup), (fun s -> s >= s_inf)

let search s_range f_test set =

  let rec search pos = function
    | [] -> raise Not_found
    | elt::_ when f_test elt -> elt,pos
    | _::q -> search (pos+1) q
  in

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec search_sup = function
    | [] -> raise Not_found
    | (s,_)::_ when s_test_sup s -> raise Not_found
    | (s,elt_list)::q when s_test s ->
        begin try
          let (elt,pos_elt) = search 1 elt_list in
          (elt,(s,pos_elt))
        with Not_found ->
          search_sup q
        end
    | _::q -> search_sup q
  in

  search_sup set

let search_and_replace s_range f_test f_replace set =

  let rec search pos = function
    | [] -> raise Not_found
    | elt::q when f_test elt -> elt,pos,(f_replace elt)@q
    | t::q ->
        let elt,pos_elt,elt_list = search (pos+1) q in
        (elt,pos_elt,t::elt_list)
  in

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec search_sup = function
    | [] -> raise Not_found
    | (s,_)::_ when s_test_sup s -> raise Not_found
    | (s,elt_list)::q when s_test s ->
        begin try
          let (elt,pos_elt,elt_list') = search 1 elt_list in
          if elt_list' = []
          then (elt,(s,pos_elt),q)
          else (elt,(s,pos_elt),(s,elt_list')::q)
        with Not_found ->
          let (elt,position,set') = search_sup q in
          elt,position,(s,elt_list)::set'
        end
    | t::q ->
        let (elt,position,set') = search_sup q in
        elt,position,t::set'
  in

  search_sup set

let search_and_replace2 s_range f_test f_replace set =

  let rec search pos = function
    | [] -> raise Not_found
    | elt::q when f_test elt ->
        let elt_list1,elt_list2 = f_replace elt in
        elt,pos,elt_list1@q, elt_list2@q
    | t::q ->
        let elt,pos_elt,elt_list1,elt_list2 = search (pos+1) q in
        (elt,pos_elt,t::elt_list1,t::elt_list2)
  in

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec search_sup = function
    | [] -> raise Not_found
    | (s,_)::_ when s_test_sup s -> raise Not_found
    | (s,elt_list)::q when s_test s ->
        begin try
          let (elt,pos_elt,elt_list1,elt_list2) = search 1 elt_list in
          let set1 = if elt_list1 = [] then q else (s,elt_list1)::q
          and set2 = if elt_list2 = [] then q else (s,elt_list2)::q in

          (elt,(s,pos_elt),set1,set2)
        with Not_found ->
          let (elt,position,set1,set2) = search_sup q in
          elt,position,(s,elt_list)::set1,(s,elt_list)::set2
        end
    | t::q ->
        let (elt,position,set1,set2) = search_sup q in
        elt,position,t::set1,t::set2
  in

  search_sup set

let for_all s_range f_test set =

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec search_sup = function
    | [] -> true
    | (s,_)::_ when s_test_sup s -> true
    | (s,elt_list)::q when s_test s ->
        if List.for_all f_test elt_list
        then search_sup q
        else false
    | _::q -> search_sup q
  in

  search_sup set

let exists s_range f_test set =

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec search_sup = function
    | [] -> false
    | (s,_)::_ when s_test_sup s -> false
    | (s,elt_list)::q when s_test s ->
        if List.exists f_test elt_list
        then true
        else search_sup q
    | _::q -> search_sup q
  in

  search_sup set

let for_all2 s_range f_test set1 set2 =

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec for_all_sub = function
    | [],[] -> true
    | [],_ | _,[] -> Debug.internal_error "[constraint.ml >> for_all2] The given two sets do not contains the same number of elements  of equal support (1)"
    | (s1,_)::_,(s2,_)::_ when s1 <> s2 -> Debug.internal_error "[constraint.ml >> for_all2] The given two sets do not contains the same number of elements  of equal support (2)"
    | (s,_)::_,_ when s_test_sup s -> true
    | (s,elt_list1)::q1, (_,elt_list2)::q2 when s_test s ->
        begin try
          (List.for_all2 f_test elt_list1 elt_list2) &&
          (for_all_sub (q1,q2))
        with Invalid_argument _ ->
          Debug.internal_error "[constraint.ml >> for_all2] The given two sets do not contains the same number of elements  of equal support (3)"
        end
    | _::q1,_::q2 -> for_all_sub (q1,q2)
  in

  for_all_sub (set1,set2)

(********* Access  *********)

let get (support,pos_elt) set =
  let rec search pos = function
    | [] -> Debug.internal_error "[constraint.ml >> get] The given position is not a position of the given set (1)"
    | elt::_ when pos = 1 -> elt
    | _::q -> search (pos-1) q
  in

  let rec search_sup = function
    | [] -> Debug.internal_error "[constraint.ml >> get] The given position is not a position of the given set (2)"
    | (s,elt_list)::_ when s = support -> search pos_elt elt_list
    | _::q -> search_sup q
  in

  search_sup set

(********* Iterators *********)

let map s_range f set =

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec map_sub = function
    | [] -> []
    | ((s,_)::_) as set' when s_test_sup s -> set'
    | (s,elt_list)::q when s_test s -> (s,List.map f elt_list)::(map_sub q)
    | t::q -> t::(map_sub q)
  in

  map_sub set

let fold_left s_range f acc set =

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec fold_sub acc' = function
    | [] -> acc'
    | ((s,_)::_) when s_test_sup s -> acc'
    | (s,elt_list)::q when s_test s -> fold_sub (List.fold_left f acc' elt_list) q
    | _::q -> fold_sub acc' q
  in

  fold_sub acc set

let iter s_range f set =

  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec iter_sub = function
    | [] -> ()
    | (s,_)::_ when s_test_sup s -> ()
    | (s,elt_list)::q when s_test s ->
        List.iter f elt_list;
        iter_sub q
    | _::q -> iter_sub q
  in

  iter_sub set

let iter2 s_range f set1 set2 =
  let s_test_sup,s_test = generate_test_support_range s_range in

  let rec iter_sub = function
    | [],[] -> ()
    | [],_ | _,[] -> Debug.internal_error "[constraint.ml >> iter2] The given two sets do not contains the same number of elements  of equal support (1)"
    | (s1,_)::_,(s2,_)::_ when s1 <> s2 -> Debug.internal_error "[constraint.ml >> iter2] The given two sets do not contains the same number of elements  of equal support (2)"
    | (s,_)::_,_ when s_test_sup s -> ()
    | (s,elt_list1)::q1, (_,elt_list2)::q2 when s_test s ->
        begin try
          List.iter2 f elt_list1 elt_list2;
          iter_sub (q1,q2)
        with Invalid_argument _ ->
          Debug.internal_error "[constraint.ml >> iter2] The given two sets do not contains the same number of elements  of equal support (3)"
        end
    | _::q1,_::q2 -> iter_sub (q1,q2)
  in

  iter_sub (set1,set2)

(********* Display *********)

let display_horizontally display_elt set =
  let result = ref "{" in
  let first = ref true in

  List.iter (fun (s,elt_list) ->
    result := !result ^ (Printf.sprintf "| s = %d -> " s);
    List.iter (fun elt ->
      let s = display_elt elt in
      if !first
      then (result := !result ^ s; first := false)
      else result := (!result ^ "; " ^ s)
    ) elt_list;
  ) set;

  !result ^ "}"

let display_vertically display_elt tab set =
  let tab' = tab ^ "  " in
  let result = ref (tab ^ "{\n") in
  let first = ref true in

  iter SAll (fun elt ->
    let s = display_elt elt in
    if !first
    then (result := !result ^ tab' ^ s; first := false)
    else result := !result ^ ";\n" ^ tab' ^ s
  ) set;

  !result ^ "\n" ^ tab ^ "}\n"

(***********************************
***              Frame           ***
************************************)

module Frame =
struct

  type dedsubterm_flag =
    | NoDedSubterm of int
    | YesDedSubterm

  type dest_flag =
    | NoDest of int
    | YesDest

  type elt =
    {
      (* General data *)
      recipe : Recipe.recipe;
      support : int;
      message : Term.term;

      (* Flags *)
      noUse : bool;
      noDedSubterm : dedsubterm_flag option;
      noDest : dest_flag option
    }

  let create r s t =
    (***[BEGIN DEBUG]***)
    Debug.high_debugging (fun () ->
      if not (Term.is_constructor_term t)
      then Debug.internal_error "[constraint.ml >> Frame.create] The term given as argument is not a constructive term."
    );
    (***[END DEBUG]***)

    { recipe = r; support = s; message = t; noUse = false; noDedSubterm = None; noDest = None }

  (********* Access *********)

  let get_recipe fc = fc.recipe

  let get_support fc = fc.support

  let get_message fc = fc.message

  (********* Modification *********)

  let replace_recipe fc rep = {fc with recipe = rep fc.recipe }

  let replace_message fc rep = {fc with message = rep fc.message }

  (********* Flags *********)

  let add_noDedSubterm fc symbol support_flag =
    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if not (Term.is_constructor symbol) || Term.is_tuple symbol
      then Debug.internal_error "[constraint.ml >> Frame.add_noDedSubterm] The function symbol should be a constructor but not a tuple";

      if Term.is_variable fc.message
      then Debug.internal_error "[constraint.ml >> Frame.add_noDedSubterm] The rule DedSubterm should not be applied on a frame constraint having a right hand side variable.";

      if fc.noUse
      then Debug.internal_error "[constraint.ml >> Frame.add_noDedSubterm] The flag NoUse is already present."
    );
    (***[END DEBUG]***)

    if Term.is_function fc.message && Term.is_equal_symbol (Term.top fc.message) symbol
    then
      match fc.noDedSubterm with
        | None -> {fc with noDedSubterm = Some(NoDedSubterm support_flag) }
        | Some(NoDedSubterm(s)) when s < support_flag -> {fc with noDedSubterm = Some(NoDedSubterm support_flag) }
        | Some(YesDedSubterm) -> Debug.internal_error (Printf.sprintf "[constraint.ml >> Frame.add_noDedSubterm] The flag YesDedSubterm should not be present")
        | _ -> fc
    else fc

  let add_yesDedSubterm fc symbol support_flag =
    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if not (Term.is_constructor symbol) || Term.is_tuple symbol
      then Debug.internal_error "[constraint.ml >> Frame.add_yesDedSubterm] The function symbol should be a constructor.";

      if Term.is_variable fc.message
      then Debug.internal_error "[constraint.ml >> Frame.add_yesDedSubterm] The rule DedSubterm should not be applied on a frame constraint having a right hand side variable.";

      if fc.noUse
      then Debug.internal_error "[constraint.ml >> Frame.add_yesDedSubterm] The flag NoUse is already present.";

      match fc.noDedSubterm with
        | Some(YesDedSubterm) when Term.is_equal_symbol symbol (Term.top fc.message) -> Debug.internal_error (Printf.sprintf "[constraint.ml >> Frame.add_yesDedSubterm] The flag YesDedSubterm should not be present")
        | Some(NoDedSubterm(s')) when s' >= support_flag && Term.is_equal_symbol symbol (Term.top fc.message)-> Debug.internal_error (Printf.sprintf "[constraint.ml >> Frame.add_yesDedSubterm] The flag NoDedSubterm(%d) should not be present when the support %d is given as argument" s' support_flag)
        | _ -> ()
    );
    (***[END DEBUG]***)

    {fc with noDedSubterm = Some(YesDedSubterm) }

  let add_noDest fc symbol support_flag =
    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if not (Term.is_destructor symbol)
      then Debug.internal_error "[constraint.ml >> Frame.add_noDest] The function symbol should be a destructor.";

      if Term.is_variable fc.message
      then Debug.internal_error "[constraint.ml >> Frame.add_noDest] The rule Dest should not be applied on a frame constraint having a right hand side variable.";

      if fc.noUse
      then Debug.internal_error "[constraint.ml >> Frame.add_noDest] The flag NoUse is already present."
    );

    Debug.high_debugging (fun () ->
      if List.exists (fun f_t -> List.exists (Term.is_equal_symbol symbol) (Term.get_projections f_t)) !Term.all_tuple
      then Debug.internal_error "[constraint.ml >> Frame.add_noDest] The destructor function symbol should not be a projection."
    );

    (***[END DEBUG]***)

    if Term.is_function fc.message
    then
      let top_f = Term.top fc.message in

      if not (Term.is_tuple top_f) && Term.link_destruc_construc symbol top_f
      then
        match fc.noDest with
          | None -> {fc with noDest = Some(NoDest support_flag) }
          | Some(NoDest(s)) when s < support_flag -> {fc with noDest = Some(NoDest support_flag) }
          | Some(YesDest) -> Debug.internal_error "[constraint.ml >> Frame.add_noDest] The flag YesDest should not be present."
          | _ -> fc
      else fc
    else fc

  let add_yesDest fc =
    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if fc.noUse
      then Debug.internal_error "[constraint.ml >> Frame.add_yesDest] The flag NoUse is already present.";

      if Term.is_variable fc.message
      then Debug.internal_error "[constraint.ml >> Frame.add_yesDest] The rule Dest should not be applied on a frame constraint having a right hand side variable."
    );
    (***[END DEBUG]***)

    { fc with noDest = Some(YesDest) }

  let add_noUse fc = { fc with noUse = true; noDedSubterm = None; noDest = None }

  let is_noUse fc = fc.noUse

  let is_yesDest fc = fc.noDest = Some(YesDest)

  let is_noDest fc support_flag = match fc.noDest with
    | Some(NoDest i) when i >= support_flag -> true
    | _ -> false

  let is_noDedSubterm fc support_flag = match fc.noDedSubterm with
  | Some(NoDedSubterm i) when i >= support_flag -> true
  | _ -> false

  let is_yesDedSubterm fc support_flag = fc.noDedSubterm = Some(YesDedSubterm)

  (********* Testing on frame *********)

  let is_same_structure frame1 frame2 =
    for_all2 SAll (fun frame_elt1 frame_elt2 ->
      Recipe.is_equal_recipe frame_elt1.recipe frame_elt2.recipe &&
      frame_elt1.support = frame_elt2.support &&
      frame_elt1.noUse = frame_elt2.noUse &&
      begin
        match frame_elt1.noDedSubterm,frame_elt2.noDedSubterm with
        | Some(YesDedSubterm),Some(YesDedSubterm) ->
            Term.is_equal_symbol (Term.top frame_elt1.message) (Term.top frame_elt2.message)
        | Some(YesDedSubterm),_ -> false
        | _, Some(YesDedSubterm) -> false
        | Some(NoDedSubterm s), Some(NoDedSubterm s') ->
            if Term.is_equal_symbol (Term.top frame_elt1.message) (Term.top frame_elt2.message)
            then s = s'
            else true
        | Some(NoDedSubterm _), _ ->
            not (Term.is_function frame_elt2.message && Term.is_equal_symbol (Term.top frame_elt1.message) (Term.top frame_elt2.message))
        | _, Some(NoDedSubterm _) ->
            not (Term.is_function frame_elt1.message && Term.is_equal_symbol (Term.top frame_elt1.message) (Term.top frame_elt2.message))
        | None, None -> true
    end &&
    begin
      match frame_elt1.noDest,frame_elt2.noDest with
        | Some(YesDest),Some(YesDest)-> true
        | Some(YesDest),_ -> false
        | _, Some(YesDest) -> false
        | Some(NoDest s), Some(NoDest s') ->
            if Term.is_equal_symbol (Term.top frame_elt1.message) (Term.top frame_elt2.message)
            then s = s'
            else true
        | Some(NoDest _), _ ->
            not (Term.is_function frame_elt2.message && Term.is_equal_symbol (Term.top frame_elt1.message) (Term.top frame_elt2.message))
        | _, Some(NoDest _) ->
            not (Term.is_function frame_elt1.message && Term.is_equal_symbol (Term.top frame_elt1.message) (Term.top frame_elt2.message))
        | None, None -> true
    end
  ) frame1 frame2

  (********* Display functions *********)

  let display_dedsubterm = function
    | NoDedSubterm(i) -> Printf.sprintf "NDed(%d)" i
    | YesDedSubterm -> Printf.sprintf "YDed"

  let display_dest = function
    | NoDest(i) -> Printf.sprintf "NDes(%d)" i
    | YesDest -> "YDes"

  let display fc =
    Printf.sprintf "%s,%d |>%s%s %s" (Recipe.display_recipe fc.recipe) fc.support (
    match fc.noDest, fc.noDedSubterm with
      | None,None -> ""
      | Some(dest), None -> Printf.sprintf "_{%s}" (display_dest dest)
      | None, Some(ded) -> Printf.sprintf "_{%s}" (display_dedsubterm ded)
      | Some(dest),Some(ded) -> Printf.sprintf "_{%s,%s}" (display_dest dest) (display_dedsubterm ded)
    )
    (if fc.noUse then "_{NoU}" else "")
    (Term.display_term fc.message)

end

(***********************************
***         Deducibility         ***
************************************)

module Deducibility =
struct

  type elt =
    {
      (* General data *)
      variable: Recipe.variable;
      message : Term.term;

      (* Flags *)
      noCons : Term.symbol list;
      noAxiom : (int * int list) list
    }

  let create var s t =
    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if s <> Recipe.get_support var
      then Debug.internal_error "[constraint.ml >> create] The support does not corresponds to the support of the variable."
    );

    Debug.high_debugging (fun () ->
      if not (Term.is_constructor_term t)
      then Debug.internal_error "[constraint.ml >> create] The term given as argument is not a constructive term."
    );
    (***[END DEBUG]***)

    { variable = var; message = t; noCons = []; noAxiom = [] }

  (********* Access *********)

  let get_recipe_variable dc = dc.variable

  let get_support dc = Recipe.get_support dc.variable

  let get_message dc = dc.message

  (********* Modification *********)

  let replace_message dc rep = {dc with message = rep dc.message }

  (********* Flags functions *********)

  let add_noCons dc f =
    (***[BEGIN DEBUG]***)
    Debug.low_debugging (fun () ->
      if List.exists (Term.is_equal_symbol f) dc.noCons
      then Debug.internal_error (Printf.sprintf "[constraint.ml >> add_noCons] The flag NoCons(%s) already exists." (Term.display_symbol_without_arity f))
    );
    (***[END DEBUG]***)

    { dc with noCons = f::dc.noCons }

  let add_noAxiom cons (s,i) =
    let rec go_through = function
      | [] -> [(s,[i])]
      | (s',pos_l)::q  when s' < s -> (s',pos_l)::(go_through q)
      | (s',pos_l)::q when s' > s -> (s,[i])::(s',pos_l)::q
      | (_,pos_l)::q -> (s, go_through_pos pos_l)::q

    and go_through_pos = function
      | [] -> [i]
      | j::q when j < i -> j::(go_through_pos q)
      | j::_ when j = i -> Debug.internal_error "[constraint.ml >> add_noAxiom] The flag already exists."
      | j::q -> i::j::q

    in

  { cons with noAxiom = go_through cons.noAxiom }

  let compare_noCons cons1 cons2 =

    let rec go_through symb_l = function
      | [] -> []
      | symb::q when List.exists (Term.is_equal_symbol symb) symb_l -> go_through symb_l q
      | symb::q -> symb::(go_through symb_l q)
    in

    go_through cons1.noCons cons2.noCons, go_through cons2.noCons cons1.noCons

  let compare_noAxiom cons1 cons2 support =

    let position1 = ref []
    and position2 = ref [] in

    let rec go_through_pos supp pos_l1 pos_l2 = match pos_l1, pos_l2 with
      | [],[] -> ()
      | _,[] -> List.iter (fun i -> position2 := (supp,i)::!position2) pos_l1
      | [],_ -> List.iter (fun i -> position1 := (supp,i)::!position1) pos_l2
      | pos1::q1,pos2::q2 when pos1 = pos2 -> go_through_pos supp q1 q2
      | pos1::q1,pos2::_ when pos1 < pos2 ->
          position2 := (supp,pos1) :: !position2;
          go_through_pos supp q1 pos_l2
      | _, pos2::q2 ->
          position1 := (supp,pos2) :: !position1;
          go_through_pos supp pos_l1 q2
    in

    let rec go_through = function
      | [],[] -> ()
      | (s1,_)::_, list2 when s1 > support -> go_through ([], list2)
      | list1, (s2,_)::_ when s2 > support -> go_through (list1, [])
      | (s1,pos_l1)::q1, [] ->
          List.iter (fun i -> position2 := (s1,i)::!position2) pos_l1;
          go_through (q1,[])
      | [], (s2,pos_l2)::q2 ->
          List.iter (fun i -> position1 := (s2,i)::!position1) pos_l2;
          go_through ([],q2)
      | (s1,pos_l1)::q1, (s2,pos_l2)::q2 when s1 = s2 ->
          go_through_pos s1 pos_l1 pos_l2;
          go_through (q1,q2)
      | (((s1,_)::_) as list1), (s2,pos_l2)::q2 when s1 > s2 ->
          List.iter (fun i -> position1 := (s2,i)::!position1) pos_l2;
          go_through (list1,q2)
      | (s1,pos_l1)::q1, list2 (* s2 > s1 *) ->
          List.iter (fun i -> position2 := (s1,i)::!position2) pos_l1;
          go_through (q1,list2)
    in

    go_through (cons1.noAxiom,cons2.noAxiom);

    !position1,!position2

  let fold_left_frame_free_of_noAxiom cons f_apply acc frame =
    let supp = Recipe.get_support cons.variable in

    let rec go_through_elt acc elt_l flag_l i = match elt_l,flag_l with
      | [],[] -> acc
      | [],_ -> Debug.internal_error "[constraint.ml >> fold_left_frame_free_of_noAxiom] Unexpected case"
      | _::q, j::q_f when i = j -> go_through_elt acc q q_f (i+1)
      | f_elt::q, _ ->
          go_through_elt (f_apply acc f_elt) q flag_l (i+1)
    in

    let rec go_through acc frame noaxiom = match frame,noaxiom with
      | [], [] -> acc
      | [],_ -> Debug.internal_error "[constraint.ml >> fold_left_frame_free_of_noAxiom] Unexpected case (2)"
      | (s,_)::_, [] when s > supp -> acc
      | (s,_)::_, _ when s > supp -> Debug.internal_error "[constraint.ml >> fold_left_frame_free_of_noAxiom] Unexpected case (3)"
      | (s,elt_l)::q_f, (s',flag_l)::q_l when s = s' ->
          go_through (go_through_elt acc elt_l flag_l 1) q_f q_l
      | (_,elt_l)::q_f, _ ->
          go_through (go_through_elt acc elt_l [] 1) q_f noaxiom
    in

    go_through acc frame cons.noAxiom

  (********* Scanning *********)

  let is_all_noCons dc =  List.length dc.noCons = !Term.number_of_constructors

  let is_noCons dc f = List.exists (Term.is_equal_symbol f) dc.noCons

  let is_same_structure set1 set2 =
    for_all2 SAll (fun cons1 cons2 ->
      Recipe.is_equal_variable cons1.variable cons2.variable
      &&
        begin try
          (List.for_all2 (fun (s1,l1) (s2,l2) ->
            (s1 = s2) && (List.for_all2 (fun p1 p2 -> p1 = p2) l1 l2)
          ) cons1.noAxiom cons2.noAxiom)
        with
          Invalid_argument _ -> false
        end
      &&
        begin try
          List.for_all2 (fun f1 f2 ->
            Term.is_equal_symbol f1 f2
          ) cons1.noCons cons2.noCons
        with
          Invalid_argument _ -> false
        end
    ) set1 set2


  let is_unsatisfiable frame cons =
    let supp = Recipe.get_support cons.variable in

    let rec go_through_elt elt_l flag_l i = match elt_l,flag_l with
      | [],[] -> true
      | [],_ -> Debug.internal_error "[constraint.ml >> is_insatisfiable] Unexpected case"
      | _::q, j::q_f when i = j -> go_through_elt q q_f (i+1)
      | f_elt::q, _ ->
          if Frame.is_noUse f_elt
            || (Recipe.occurs cons.variable f_elt.Frame.recipe)
            || not (Term.is_unifiable [f_elt.Frame.message,cons.message])
          then go_through_elt q flag_l (i+1)
          else false
    in

    let rec go_through frame noaxiom = match frame,noaxiom with
      | [], [] -> true
      | [],_ -> Debug.internal_error "[constraint.ml >> is_insatisfiable] Unexpected case (2)"
      | (s,elt_l)::q_f, (s',flag_l)::q_l when s = s' ->
          if go_through_elt elt_l flag_l 1
          then go_through q_f q_l
          else false
      | (s,_)::_, [] when s > supp -> true
      | (s,_)::_, _ when s > supp -> Debug.internal_error "[constraint.ml >> is_insatisfiable] Unexpected case (3)"
      | (_m,elt_l)::q_f, _ ->
          if go_through_elt elt_l [] 1
          then go_through q_f noaxiom
          else false
    in

    (***[BEGIN DEBUG]***)
    Debug.high_debugging (fun () ->
      if exists (SUntil supp) (fun fc ->
          not (Frame.is_noUse fc) &&
          Term.is_function fc.Frame.message &&
          (List.exists (Term.is_equal_symbol (Term.top fc.Frame.message)) ([Term.senc; Term.aenc; Term.sign]@ !Term.all_tuple)) &&
          not (Frame.is_yesDest fc) &&
          not (Frame.is_noDest fc supp)
        ) frame
      then Debug.internal_error "[constraint.ml >> is_unsatisfiable] The rule Dest is not useless on the given frame."
    );
    (***[END DEBUG]***)

  (* Useless Cons *)
  (
    (Term.is_variable cons.message && List.length cons.noCons = !Term.number_of_constructors)
    || (Term.is_name cons.message)
    || (Term.is_function cons.message && List.exists (Term.is_equal_symbol (Term.top cons.message)) cons.noCons)
  ) &&
  (* Useless Axiom *)
  (go_through frame cons.noAxiom)

  (********* Display *********)

  let rec display_noCons = function
    | [] -> ""
    | [x] -> Term.display_symbol_without_arity x
    | t::q -> (Term.display_symbol_without_arity t)^", "^(display_noCons q)

  let rec display_noAxiom = function
    | [] -> ""
    | (s,elt_l)::q -> (List.fold_left (fun acc i -> Printf.sprintf "%s, (%d,%d)" acc s i) "" elt_l)^(display_noAxiom q)

  let display cons =
    Printf.sprintf "%s,%d |-%s%s %s" (Recipe.display_variable cons.variable) (Recipe.get_support cons.variable)
    (if cons.noCons <> [] then "{"^(display_noCons cons.noCons)^"}" else "")
    (if cons.noCons <> [] then "{"^(display_noAxiom cons.noAxiom)^"}" else "")
    (Term.display_term cons.message)
end

let is_subset_noUse la frame =
  let test_one ax1 elt =	(* outputs true only if elt is an axiom = ax1 *)
    let recipe = Frame.get_recipe elt in
    match Recipe.axiom_of_recipe recipe with
    | Some ax2 -> Recipe.is_equal_axiom ax2 ax1
    | None -> false in
  let rec aux_scan = function
    | [] -> true
    | ax :: la' ->
       begin
	 try let elt,_ = search SAll (test_one ax) frame (* SAll: all elements in frame *)
	     in if Frame.is_noUse elt
		then aux_scan la'
		else false
	 with Not_found ->
	   Debug.internal_error "[constraint.ml >> is_subset_noUse] Called with a list of axioms whose at least one element in not in the frame."
       end in
  aux_scan la
