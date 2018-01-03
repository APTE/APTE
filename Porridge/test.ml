
(** Utilities for registering tests and running them *)

let h = Hashtbl.create 5

let register key title x =
  assert (not (Hashtbl.mem h key)) ;
  Hashtbl.add h key (title,x)

(** Command line options *)
module Arg = struct

  (** Selected test *)
  let key = ref ""

  let succ = ref false
  let self = ref true
  let equivalence = ref false
  let persistent = ref false
  let sleep = ref false
  let twoSides = ref true
  let names = ref true
  let size = ref false
  let bench = ref false
  let traces = ref (-1)

  let options = [
    "--succ", Arg.Set succ, "select successor states" ;
    "--no-self", Arg.Clear self, "do not select initial state" ;
    "--persistent", Arg.Set persistent,
      "select persistent LTS and compute persistent sets for selected sets" ;
    "--sleep", Arg.Set sleep, "select sleep LTS" ;
    "--equivalence", Arg.Set equivalence,
      "force selection of trace equiv. LTS when reduced systems are selected" ;
    "--twoSides", Arg.Set twoSides,
      "consider non-zero process on the right" ;
    "--fresh-names", Arg.Set names,
      "for toy example, consider distinct names on both sides" ;
    "--size", Arg.Set size, "compute LTS size in selected LTSs" ;
    "--traces", Arg.Unit (fun () -> traces := max_int),
      "show maximal traces in selected LTSs" ;
    "--bench", Arg.Set bench,
      "slitently compute set of traces to explore" ;
    "--traces-up-to", Arg.Int (fun i -> traces := i),
      "show traces of length <n> in selected LTSs"
  ]

  let usage () =
    Format.asprintf
      "Usage: test [options] <example>\n\n\
       List of available examples:\n\
       %a\n\
       List of available options:"
      (fun ch h ->
         Hashtbl.iter
           (fun k (title,_) ->
              Format.fprintf ch "  %s : %s\n" k title)
           h)
      h

  let run () =
    Arg.parse options (fun s -> key := s) (usage ()) ;
    if !traces >= 0 then size := true ;
    equivalence := !equivalence || not (!sleep || !persistent) ;
    if !key = "" then begin
      Arg.usage options (usage ()) ;
      exit 1
    end ;
    try
      let (t,x) = Hashtbl.find h !key in
        Format.printf "*** %s ***@.@." t ;
        x
    with Not_found -> Format.printf "Invalid example name.@." ; exit 1

end

(** Main function *)

open Sem_utils
open Frame

module POR = POR.Make(Trace_equiv)
module Persistent = POR.Persistent
module Sleep = POR.Sleep

let main s =

  let module S = Trace_equiv in

  let pp_persistent ch s =
    let t0 = Unix.gettimeofday () in
    let set = POR.persistent s in
      Format.fprintf ch "%a (%.2fs)"
        S.ActionSet.pp set (Unix.gettimeofday () -. t0) ;
  in

  (* Some transitions and persistent set computations *)
  Format.printf "@[<2>s =@ %a@]@.@." S.State.pp s ;
  if !Arg.succ then
  S.fold_successors s ()
    (fun a s' () ->
         Format.printf
           "@[<2>s -%a->@ s'=%a@]@."
           S.Action.pp a
           S.State.pp s' ;
         if !Arg.persistent then
         Format.printf
           "P(s') = %a@.@."
           pp_persistent s') ;
  if !Arg.self && !Arg.persistent then
  Format.printf
    "P(s) = %a@."
    pp_persistent s ;

  (* Number of states and traces in successive transition systems *)

  Format.printf "@." ;

  if !Arg.size then begin

    if !Arg.equivalence then begin
    let module Stats = LTS.Make(Trace_equiv) in
    Format.printf "Equivalence LTS: %d states, %d traces.\n"
      (Stats.StateSet.cardinal (Stats.reachable_states s))
      (Stats.nb_traces s) ;
    if !Arg.traces >=0 then begin
      Format.printf "@.Traces:@." ;
      Stats.show_traces ~bound:!Arg.traces s
    end
    end ;

    if !Arg.persistent then begin
    let module Stats = LTS.Make(Persistent) in
    Format.printf "Persistent: %d states, %d traces.\n"
      (Stats.StateSet.cardinal (Stats.reachable_states s))
      (Stats.nb_traces s) ;
    if !Arg.traces >= 0 then begin
      Format.printf "@.Traces:@." ;
      Stats.show_traces ~bound:!Arg.traces s
    end
    end ;

    if !Arg.sleep then begin
    let module Stats = LTS.Make(Sleep) in
    let s = s,S.Z.empty in
    if !Arg.bench then
      let _ = Stats.traces s in
      Printf.printf "Computation of reduced set of traces: Done.";
    else Format.printf "Sleep: %d states, %d traces.\n"
        (Stats.StateSet.cardinal (Stats.reachable_states s))
        (Stats.nb_traces s) ;
      if !Arg.traces >= 0 then begin
        Format.printf "@.Traces:@." ;
        Stats.show_traces ~bound:!Arg.traces s
      end
    end

  end ;

  Format.printf "@."

(** Test instances *)

let io c p = Process.input c (Term.var "x") (Process.output c (Term.var "x") p)
let iosenc c p = Process.input c (Term.var "y") (Process.output c (Term.senc (Term.var "y") (Term.var "y")) p)

let p = io Channel.c Process.zero
let q = io Channel.d Process.zero
let r = io Channel.e Process.zero
let psenc = iosenc Channel.c Process.zero
let qsenc = iosenc Channel.d Process.zero
let rsenc = iosenc Channel.e Process.zero

let ok = Term.ok ()

let inp c  = Process.input c (Term.var "x") Process.zero
let out c  = Process.output c (Term.var "x") Process.zero
let outout c d = Process.output c (Term.var "x") (Process.output d (Term.var "y") Process.zero)

let () =

  register "ex1" "(in(c) | out(d))"
    (Trace_equiv.State.of_process (Process.par [inp Channel.c; out Channel.d]));

  register "ex2" "(in(c) | out(d) |out(e))"
    (Trace_equiv.State.of_process (Process.par [inp Channel.c; out Channel.d; out Channel.e]));

  (** This example relies on a dependency that involves ghosts:
    * with out(c) as the seed, we explore the independent transition out(d)
    * but find that out(e) is dependent with out(c);
    * with out(d) as seed, the computation stops immediately.
    * The dependency is independent of the ghost numbering / age scheme,
    * but relies on different ghost frames. *)
  register "ex3" "(out(c)| out(d).out(e)) +  (out(c)|out(d))"
    (Trace_equiv.State.of_process (Process.plus
                                     [Process.par [out Channel.c; outout Channel.d Channel.e];
                                      Process.par [out Channel.c; out Channel.d]]));

  register "ex4" "c|de = c|d"
    (Trace_equiv.State.update Trace_equiv.State.empty
       ~left:(Configs.singleton (Process.par [out Channel.c; outout Channel.d Channel.e], Frame.empty))
       ~right:(Configs.singleton (Process.par [out Channel.c; out Channel.d], Frame.empty)));

  register "ex5" "ioc | d | d"
    (Trace_equiv.State.of_process (Process.par [p;out Channel.d; out Channel.d])) ;

  register "ex6" "(c | ioc | iod) + (c | ioc | iod)"
    (Trace_equiv.State.of_process (Process.plus [Process.par [out Channel.c; psenc; q];
                                                 Process.par [out Channel.c; p;q]]));

  register "ex7" "(ioc | ioc | iod | iod)"
    (Trace_equiv.State.of_process  (Process.par [p;p; q;q]));

  register "ex8" "(ioc | ioc | ioc | iod | iod | iod)"
    (Trace_equiv.State.of_process  (Process.par [p;p;p;q; q;q]));

  register "ex9" "(ioc | ioc | iod | iod) = (ioc | ioc | iod | iod)"
    (Trace_equiv.State.update Trace_equiv.State.empty
       ~left:(Configs.singleton (Process.par [p;p;q;q], Frame.empty))
       ~right:(Configs.singleton (Process.par [p;p;q;q], Frame.empty)));

  register "ex10" "(iocioc | iocioc | iodiod | iodiod)"
    (Trace_equiv.State.of_process
       (let open Channel in Process.par [io c (io c Process.zero);
                                         io c (io c Process.zero);
                                         io d (io d Process.zero);
                                         io d (io d Process.zero)]));

  register "det" "Action-deterministic (3 parallel IO sequences)"
    (Trace_equiv.State.of_process (Process.par [p;q;r])) ;

  register "det2" "Action-deterministic (3 parallel IO^2 sequences)"
    (Trace_equiv.State.of_process (let open Channel in
                   Process.par [ io c (io c Process.zero) ;
                                 io d (io d Process.zero) ;
                                 io e (io e Process.zero) ])) ;

  register "det3" "Action-deterministic (3 parallel IO^3 sequences)"
	   (Trace_equiv.State.of_process (let open Channel in
					  Process.par [ io c (io c (io c Process.zero)) ;
							io d (io d (io d Process.zero)) ;
							io e (io e (io e Process.zero)) ])) ;

  let () = 
    (* Toy examples *)
    let count = ref 0 in
    let proc c n =
      Process.(
        input c (Term.var "x") (if_eq (Term.var "x") ok (output c n zero) zero)
      ) in
    let procName pars () = Process.par
			     (List.map (fun (c,s) ->
					incr(count);
					proc c (Term.var (Printf.sprintf "%s_%d" s !count)))
				       pars) in
    register "toy3"
        "Action-deterministic toy example with 3 parallel in(x).[x=ok].out(n)"
        (let p = procName [ Channel.c, "nc" ;
                            Channel.d, "nd" ;
                            Channel.e, "ne" ] in
	 let p1, p2 = if !Arg.twoSides
		      then (if !Arg.names then (p (), p())
			    else let p = p () in (p, p))
		      else  (p (), Process.zero) in
	 Trace_equiv.State.of_processes p1 p2) ;
      register "toy4"
               "Action-deterministic toy example with 4 parallel in(x).[x=ok].out(n)"
        (let p = procName [ Channel.c, "nc" ;
                            Channel.d, "nd" ;
                            Channel.e, "ne" ;
                            Channel.f, "nf" ] in
	 let p1, p2 = if not(!Arg.twoSides) then (p (), Process.zero)
		      else if !Arg.names then p (), p()
		      else let pr = p () in pr, pr in
	 Trace_equiv.State.of_processes p1 p2) ;
      register "toy5"
        "Action-deterministic toy example with 5 parallel in(x).[x=ok].out(n)"
        (let p = procName [ Channel.c, "nc" ;
                            Channel.d, "nd" ;
                            Channel.e, "ne" ;
                            Channel.f, "nf" ;
                            Channel.g, "ng" ] in
	 let p1, p2 = if not(!Arg.twoSides) then p(), Process.zero
		      else if !Arg.names then p (), p()
		      else let p1 = p () in p1, p1 in
	 Trace_equiv.State.of_processes p1 p2) ;
      register "toy6"
        "Action-deterministic toy example with 6 parallel in(x).[x=ok].out(n)"
        (let p = procName [ Channel.c, "nc" ;
                            Channel.d, "nd" ;
                            Channel.e, "ne" ;
                            Channel.f, "nf" ;
                            Channel.g, "ng" ;
                            Channel.h, "nh" ] in
	 let p1, p2 = if not(!Arg.twoSides) then p(), Process.zero
		      else if !Arg.names then p (), p()
		      else let p1 = p () in p1, p1 in
	 Trace_equiv.State.of_processes p1 p2)
  in

  register "ndet" "NOT action-deterministic"
    (Trace_equiv.State.of_process
       (Process.par [p;p;q;q;Process.output Channel.d ok q]))

let () =
  let open Process in
  let p c = input c (Term.var "y") (input c (Term.var "z")
             (output c (Term.ok ()) (output c (Term.ok ()) zero))) in
  let s = Process.par [p Channel.c; p Channel.d] in
  let s = Trace_equiv.State.of_process s in
    register "sleep" "Action-deterministic with non-trivial blocks (IIOOc | IIOOd)" s

(** ... *)

let () = main (Arg.run ())
