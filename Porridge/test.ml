(** Utilities for registering tests and running them *)

let h = Hashtbl.create 5

let register key title x = Hashtbl.add h key (title,x)

(** Command line options *)
module Arg = struct

  (** Selected test *)
  let key = ref ""

  let succ = ref false
  let self = ref true
  let equivalence = ref false
  let persistent = ref false
  let sleep = ref false
  let size = ref false
  let traces = ref (-1)

  let options = [
    "--succ", Arg.Set succ, "select successor states" ;
    "--no-self", Arg.Clear self, "do not select initial state" ;
    "--persistent", Arg.Set persistent,
      "select persistent LTS and compute persistent sets for selected sets" ;
    "--sleep", Arg.Set sleep, "select sleep LTS" ;
    "--equivalence", Arg.Set equivalence,
      "force selection of trace equiv. LTS when reduced systems are selected" ;
    "--size", Arg.Set size, "compute LTS size in selected LTSs" ;
    "--traces", Arg.Unit (fun () -> traces := max_int),
      "show maximal traces in selected LTSs" ;
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
      Format.printf "Sleep: %d states, %d traces.\n"
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
let p = io Channel.c Process.zero
let q = io Channel.d Process.zero
let r = io Channel.e Process.zero
let ok = Term.ok ()

let () =

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
