open Trace_equiv
open Sem_utils
module POR = POR.Make(Trace_equiv)

let () =
  Check.add_suite
    ("POR",
     [
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
          let st p =
            { State.
              left = Configs.of_process p ;
              right = Configs.empty ;
              constraints = Constraints.empty }
          in
            Alcotest.(check (module ActionSet))
              "persistent set for Oc"
              (ActionSet.singleton (Action.Out (Channel.c,0)))
              (POR.persistent (st oc)) ;
            let s = st (Process.par [ioc;iod]) in
            Alcotest.(check (module ActionSet))
              "persistent set for (IOc|IOd)"
              (enabled_cover s)
              (POR.persistent s) ;
            Alcotest.(check int)
              "persistent set for (Ic|Id)"
              1
              (ActionSet.cardinal
                 (POR.persistent (st (Process.par [ic;id])))) ;
            Alcotest.(check (module ActionSet))
              "persistent set for (IOd|Oc)"
              (ActionSet.singleton (Action.Out (Channel.c,0)))
              (POR.persistent (st (Process.par [iod;oc]))) ;
            (* non-determinism forces a degenerate persistent set *)
            let s = st (Process.par [ioc;oc]) in
            Alcotest.(check (module ActionSet))
              "persistent set for (IOc|Oc)"
              (enabled_cover s)
              (POR.persistent s) ;
       ) ;
     ])

let () = Check.run ()
