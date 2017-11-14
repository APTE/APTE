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
            Process_.input Channel.c (Term_.var "x")
              (Process_.output Channel.c (Term_.var "x")
                 Process_.zero)
          in
          let iod =
            Process_.input Channel.d (Term_.var "x")
              (Process_.output Channel.d (Term_.var "x")
                 Process_.zero)
          in
          let ic = Process_.input Channel.c (Term_.var "x") Process_.zero in
          let id = Process_.input Channel.d (Term_.var "x") Process_.zero in
          let oc = Process_.output Channel.c (Term_.ok ()) Process_.zero in
          let st p =
            State.make
              ~left:(Configs.of_process p)
              ~right:Configs.empty
              ~constraints:Constraints.empty
          in
            Alcotest.(check (module ActionSet))
              "persistent set for Oc"
              (ActionSet.singleton (Action.Out (Channel.c,0)))
              (POR.persistent (st oc)) ;
            let s = st (Process_.par [ioc;iod]) in
            Alcotest.(check (module ActionSet))
              "persistent set for (IOc|IOd)"
              (enabled_cover s)
              (POR.persistent s) ;
            Alcotest.(check int)
              "persistent set for (Ic|Id)"
              1
              (ActionSet.cardinal
                 (POR.persistent (st (Process_.par [ic;id])))) ;
            Alcotest.(check (module ActionSet))
              "persistent set for (IOd|Oc)"
              (ActionSet.singleton (Action.Out (Channel.c,0)))
              (POR.persistent (st (Process_.par [iod;oc]))) ;
            (* non-determinism forces a degenerate persistent set *)
            let s = st (Process_.par [ioc;oc]) in
            Alcotest.(check (module ActionSet))
              "persistent set for (IOc|Oc)"
              (enabled_cover s)
              (POR.persistent s) ;
       ) ;
     ])

let () = Check.run ()
