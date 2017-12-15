open Trace_equiv
open Sem_utils
open Frame

module POR = POR.Make(Trace_equiv)

let saset_of_aset s =
  ActionSet.fold SemanticActionSet.add s SemanticActionSet.empty

let () =
  Check.add_suite
    ("POR",
     [
       "fc(IOd|Oc,Id)", `Quick,
       begin fun () ->
         let iod =
           Process.input Channel.d (Term.var "x")
             (Process.output Channel.d (Term.var "x")
                Process.zero)
         in
         let oc =
           Process.output Channel.c (Term.ok ()) Process.zero
         in
         let fc =
           POR.first_conflicts
             (Action.In (Channel.d,0,Domain.empty))
             (State.of_process (Process.par [iod;oc]))
         in
           Format.printf "@[Result = %a@]@." SemanticActionSet.pp fc ;
           Alcotest.(check int)
             "cardinal"
             1
             (SemanticActionSet.cardinal fc) ;
           Alcotest.(check bool)
             "contains only inputs on d"
             true
             (SemanticActionSet.for_all
                (function
                   | Action.In (c,_,_) when c = Channel.d -> true
                   | _ -> false)
                fc)
       end ;

       (** Check that fc(s,a) does not consider symbolic executions
         * which may have concretizations starting with a concretization
         * of a. *)
       "fc(IOIOc,Ic)", `Quick,
       begin fun () ->
         let ioioc =
           let c = Channel.c and x = Term.var "x" in
             Process.(input c x (output c x (input c x (output c x zero))))
         in
         let phi =
           Frame.append Frame.empty Channel.d (Term.ok ())
         in
         let fc =
           POR.first_conflicts
             (Action.In (Channel.c,0,Domain.empty))
             (State.update State.empty
                ~left:(Configs.add (ioioc,phi) Configs.empty))
         in
           Format.printf "@[Result = %a@]@." SemanticActionSet.pp fc ;
           Alcotest.(check int)
             "cardinal"
             1
             (SemanticActionSet.cardinal fc)
       end ;

       (** A case of first_enabling for an input action where the
         * correct result should not only contain the missing outputs. *)
       "fe(in(c) + (in(d).in(c) | out(c)),in(c,{w_c^0=ok))", `Quick,
       begin fun () ->
         let p =
           Process.plus
             [ Process.input Channel.c (Term.var "x") Process.zero ;
               Process.par
                 [ Process.input Channel.d (Term.var "x")
                     (Process.input Channel.c (Term.var "x")
                        Process.zero) ;
                   Process.output Channel.c (Term.ok ())
                     Process.zero ] ]
         in
         let s = State.of_process p in
         let dom = Domain.add Domain.empty Channel.c in
         let fe =
           Format.printf "Testing on %a.@." State.pp s ;
           POR.first_enabling
             (Action.In (Channel.c,0,dom))
             s
         in
           Format.printf "fe(_) = %a@." SemanticActionSet.pp fe ;
           Alcotest.(check int)
             "nb of executable actions"
             3
             (ActionSet.cardinal (enabled_cover s)) ;
           Alcotest.(check int)
             "cardinal"
             2
             (SemanticActionSet.cardinal fe) ;
           Alcotest.(check bool)
             "contains one out(c,0)"
             true
             (SemanticActionSet.exists
                (function
                   | Action.Out (c,0) when c = Channel.c -> true
                   | _ -> false)
                fe) ;
           Alcotest.(check bool)
             "contains one input on d"
             true
             (SemanticActionSet.exists
                (function
                   | Action.In (d,0,dom') ->
                       d = Channel.d && dom' = dom
                   | _ -> false)
                fe)
       end ;

       (** Another case of first_enabling for an input action where the
         * correct result should not only contain the missing outputs.
         * Note: this test used to be relevant when input actions did not
         * have sequence numbers, it is now a bit trivial. *)
       "fe(in(c).[out(d) | in(e).in(c)])", `Quick,
       begin fun () ->
         let x = Term.var "x" in
         let ok = Term.ok () in
         let p = let open Process in let open Channel in
           input c x
             (par [ output d ok zero ;
                    input e x (input c x zero) ])
         in
         let dom = Domain.add Domain.empty Channel.d in
           Alcotest.check (module SemanticActionSet)
             "equal"
             (SemanticActionSet.singleton (Action.Out (Channel.d,0)))
             (POR.first_enabling
                (Action.In (Channel.c,0,dom))
                (State.of_process p)) ;
           Alcotest.(check bool)
             "contains in(e)"
             true
             (SemanticActionSet.exists
                (function
                   | Action.In (x,0,dom') -> x = Channel.e && dom' = dom
                   | _ -> false)
                (POR.first_enabling
                   (Action.In (Channel.c,1,dom))
                   (State.of_process p)))
       end ;

       "Persistent sets", `Quick,
       (fun () ->
          let ioc =
            Process.input Channel.c (Term.var "x")
              (Process.output Channel.c (Term.var "x")
                 Process.zero)
          in
          let iod =
            Process.input Channel.d (Term.var "x")
              (Process.output Channel.d (Term.var "x")
                 Process.zero)
          in
          let ic = Process.input Channel.c (Term.var "x") Process.zero in
          let id = Process.input Channel.d (Term.var "x") Process.zero in
          let oc = Process.output Channel.c (Term.ok ()) Process.zero in
            Alcotest.(check (module ActionSet))
              "persistent set for Oc"
              (ActionSet.singleton (Action.Out (Channel.c,0)))
              (POR.persistent (State.of_process oc)) ;
            let s = State.of_process (Process.par [ioc;iod]) in
            Alcotest.(check (module SemanticActionSet))
              "persistent set for (IOc|IOd)"
              (saset_of_aset (enabled_cover s))
              (saset_of_aset (POR.persistent s)) ;
            let ps = POR.persistent (State.of_process (Process.par [ic;id])) in
            Format.printf "@[Result = %a@]@." ActionSet.pp ps ;
            Alcotest.(check int)
              "persistent set for (Ic|Id)"
              1
              (ActionSet.cardinal ps) ;
            Alcotest.(check (module ActionSet))
              "persistent set for (IOd|Oc)"
              (ActionSet.singleton (Action.Out (Channel.c,0)))
              (POR.persistent (State.of_process (Process.par [iod;oc]))) ;
            (* non-determinism forces a degenerate persistent set *)
            let s = State.of_process (Process.par [ioc;oc]) in
            Alcotest.(check (module SemanticActionSet))
              "persistent set for (IOc|Oc)"
              (saset_of_aset (enabled_cover s))
              (saset_of_aset (POR.persistent s)) ;
       ) ;

       "Persistent sets with conditionals", `Quick,
       begin fun () ->
         let p =
           Process.par [
             Process.input Channel.c (Term.var "x")
               (Process.if_eq (Term.var "x") (Term.var "y")
                  (Process.output Channel.c (Term.var "y") Process.zero)
                  Process.zero) ;
             Process.input Channel.d (Term.var "x")
               Process.zero
           ]
         in
         Alcotest.(check int)
           "persistent set for (I[test]Oc|Id)"
           1
           (ActionSet.cardinal (POR.persistent (State.of_process p))) ;
         let p =
           Process.par [
             Process.input Channel.c (Term.var "x")
               (Process.if_eq (Term.var "x") (Term.var "y")
                  (Process.output Channel.c (Term.var "y") Process.zero)
                  Process.zero) ;
             Process.input Channel.d (Term.var "x")
               (Process.if_eq (Term.var "x") (Term.var "y")
                  (Process.output Channel.d (Term.var "y") Process.zero)
                  Process.zero) ;
           ]
         in
         Alcotest.(check int)
           "persistent set for (I[test]Oc|I[test]d)"
           2
           (ActionSet.cardinal (POR.persistent (State.of_process p))) ;
       end ;

       (* Check that sleep sets do not kill parallel inputs.
        * TODO extend to check that in(c).in(d) and in(d).in(c)
        * cannot both be executed without a dependency constraints *)
       "Sleep set allowing inputs", `Quick,
       begin fun () ->
         let x = Term.var "x" in
         let p = let open Process in let open Channel in
           par [ input c x (output c x zero) ;
                 input d x (output d x zero) ]
         in
         let s = State.of_process p in
         let has p =
           POR.Sleep.fold_successors (s,Z.empty) false
             (fun a s' b -> b || p a s')
         in
           Alcotest.(check bool)
             "has input on c"
             true
             (has
                (fun a _ -> match a with
                   | ZAction.Input (x,_,_) when x = Channel.c -> true
                   | _ -> false)) ;
           Alcotest.(check bool)
             "has input on d"
             true
             (has
                (fun a _ -> match a with
                   | ZAction.Input (x,_,_) when x = Channel.d -> true
                   | _ -> false))
       end ;

       "Sleep set constraining outputs", `Quick,
       begin fun () ->
         let p c =
           Process.par [
             Process.output c (Term.ok ()) Process.zero ;
             Process.input c (Term.var "x")
               (Process.output c (Term.var "z") Process.zero)
           ]
         in
         let s = State.of_process (Process.par [p Channel.c; p Channel.d]) in
         let has s p =
           POR.Sleep.fold_successors s false
             (fun a s' b -> b || p a s')
         in
         let has_out s c =
           has s (fun a _ -> match a with
                    | ZAction.Output x when x = c -> true
                    | _ -> false)
         in
           Alcotest.(check (module SemanticActionSet))
             "trivial persistent set"
             (saset_of_aset (enabled_cover s))
             (saset_of_aset (POR.persistent s)) ;
           Alcotest.(check bool)
             "(s,ø) can output on c"
             true
             (has_out (s,Z.empty) Channel.c) ;
           Alcotest.(check bool)
             "(s,ø) can output on d"
             true
             (has_out (s,Z.empty) Channel.d) ;
           Alcotest.(check bool)
             "(s,ø) cannot perform both out(c).out(d) and out(d).out(c)"
             false
             (has
                (s,Z.empty)
                (fun a s' -> match a with
                   | ZAction.Output x when x = Channel.c ->
                       has_out s' Channel.d
                   | _ -> false)
              &&
              has
                (s,Z.empty)
                (fun a s' -> match a with
                   | ZAction.Output x when x = Channel.d ->
                       has_out s' Channel.c
                   | _ -> false))
       end ;
     ])

let () = Check.run ()
