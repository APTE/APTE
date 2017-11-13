(** Utilities for registering tests and running them *)

let h = Hashtbl.create 5

let register key title f x =
  Hashtbl.add h key (title,f,x)

let run () =
  if Array.length Sys.argv = 1 then begin
    Format.printf "Usage: test <key>@." ;
    Format.printf "with <key> among the following options:@." ;
    Hashtbl.iter
      (fun k (title,_,_) ->
         Format.printf "  %s : %s@." k title)
      h
  end else begin
    let (t,f,x) = Hashtbl.find h Sys.argv.(1) in
      Format.printf "*** %s ***@.@." t ;
      f x
  end 

(** Testing functions *)

open Porridge

module POR = POR.Make(Trace_equiv)
module Persistent = POR.Persistent
module Sleep = POR.Sleep

let test ?(show_sleep=false) s =

  let module S = Trace_equiv in

  let pp_persistent ch s =
    let t0 = Unix.gettimeofday () in
    let set = POR.persistent s in
      Format.fprintf ch "%a (%.2fs)"
        S.ActionSet.pp set (Unix.gettimeofday () -. t0) ;
  in

  (* Some transitions and persistent set computations *)
  Format.printf "s = %a@.@." S.State.pp s ;
  S.fold_successors s ()
    (fun a s' () ->
         Format.printf
           "s -%a-> s'=%a@."
           S.Action.pp a
           S.State.pp s' ;
         Format.printf
           "P(s') = %a@.@."
           pp_persistent s') ;
  Format.printf
    "P(s) = %a@."
    pp_persistent s ;

  (* Number of states and traces in successive transition systems *)

  Format.printf "@." ;

  let module Stats = LTS.Make(Trace_equiv) in
  Format.printf "Equivalence LTS: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;

  let module Stats = LTS.Make(Persistent) in
  Format.printf "Persistent: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;
  let module Stats = LTS.Make(Sleep) in

  let s = Sleep.from_state s in
  Format.printf "Sleep: %d states, %d traces.\n"
    (Stats.StateSet.cardinal (Stats.reachable_states s))
    (Stats.nb_traces s) ;
  if show_sleep then
    Stats.show_traces s ;

  Format.printf "@."

(** Test instances *)

let make_state p =
  { Trace_equiv.State.
    left = Sem_utils.Configs.of_process p ;
    right = Sem_utils.Configs.empty ;
    constraints = Sem_utils.Constraints.empty }

let io c p = Process.input c (Term.var "x") (Process.output c (Term.var "x") p)
let p = io Channel.c Process.zero
let q = io Channel.d Process.zero
let r = io Channel.e Process.zero
let ok = Term.ok ()

let () =

  register "det" "Action-deterministic"
    test
    (make_state (Process.par [p;q;r])) ;

  (* [p;p;q;q;Process.output Channel.d ok q] *)
  register "ndet" "NOT action-deterministic"
    test
    (make_state (Process.par [p;p;q;q;q]))

let () =
  let open Process in
  let p c = input c (Term.var "y") (input c (Term.var "z")
             (output c (Term.ok ()) (output c (Term.ok ()) zero))) in
  let s = Process.par [p (Channel.of_int 0); p (Channel.of_int 1)] in
  let s = make_state s in
    register "sleep" "Sleep details" (test ~show_sleep:true) s

(** ... *)

let () = run ()
