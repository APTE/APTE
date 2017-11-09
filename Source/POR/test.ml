
module Base = Semantics.Powerset
module POR = POR.Make(Base)
module Persistent = POR.Persistent
module Sleep = POR.Sleep

let test ?(show_sleep=false) s =

  (* Some transitions and persistent set computations *)
  let module S = Semantics.Powerset in
  S.ActionMap.iter
    (fun a s' ->
         Printf.printf
           "%s -[%s]-> %s\n"
           (S.State.to_string s)
           (S.Action.to_string a)
           (S.State.to_string s') ;
         Printf.printf
           "P(%s) = %s\n"
           (S.State.to_string s')
           (S.ActionSet.to_string (POR.persistent s')))
    (S.successors s) ;
  Printf.printf
    "P(%s) = %s\n"
    (S.State.to_string s)
    (S.ActionSet.to_string (POR.persistent s)) ;

  (* Number of states and traces in successive transition systems *)
  let module Stats = LTS.Make(Base) in
  Printf.printf "Original powerset: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;
  let module Stats = LTS.Make(Persistent) in
  Printf.printf "Persistent: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;
  let module Stats = LTS.Make(Sleep) in
  let s = Sleep.from_state s in
  Printf.printf "Sleep: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;
  if show_sleep then
    Stats.show_traces s

let io c p = Process.input c () (Process.output c () p)
let p = io 'c' Process.zero
let q = io 'd' Process.zero
let r = io 'e' Process.zero

let () =
  let s = Process.par [p;q;r] in
  let s = Semantics.StateSet.singleton (s,0) in
    Printf.printf "*** Action-deterministic ***\n" ;
    test s

let () =
  let s = Process.par [p;p;q;q;(Process.output 'd' () q)] in
  let s = Semantics.StateSet.singleton (s,0) in
    Printf.printf "*** NOT action-deterministic ***\n" ;
    test s

let () =
  let open Process in
  let p c = input c () (input c () (output c () (output c () zero))) in
  let s = Process.par [p 'c'; p 'd'] in
  let s = Semantics.StateSet.singleton (s,0) in
    Printf.printf "*** Sleep details ***\n" ;
    test ~show_sleep:true s
