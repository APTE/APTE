(********************
***      Help     ***
*********************)

let print_help () = 
  Printf.printf "Name : APTE\n";
  Printf.printf "   Algorithm for Proving Trace Equivalence\n\n";
  Printf.printf "Version 0.4beta\n\n";
  Printf.printf "Synopsis :\n";
  Printf.printf "      apte [-debug high|low|none] [-unfold] [-no_comm] [-no_erase] [-verbose [<int>]]\n";
  Printf.printf "           [-display size|step] [-log <int>] file\n\n";
  Printf.printf "Options :\n";
  Printf.printf "      -debug [high|low|none] : APTE is programmed with three level of debugging.\n";
  Printf.printf "          The High debugging option checks several invariants of the algorithms.\n";
  Printf.printf "          While this mode provides more guarantee about the result, it makes the\n";
  Printf.printf "          algorithm slower.\n";
  Printf.printf "          The Low debugging option only checks basic invariants. \n";
  Printf.printf "          The None debugging option does not check any invariant. (default) Chose this option\n";
  Printf.printf "          for optimal running time.\n\n";
  Printf.printf "      -display step : Show statistics on the matrices generated by APTE for each\n";
  Printf.printf "          main step of the algorithm.\n\n";
  Printf.printf "      -display size : Group the statistics on the matrices generated by the size\n";
  Printf.printf "          traces.\n\n";
  Printf.printf "      -log <int> : Log all the symbolic processes and the matrices obtained on the\n";
  Printf.printf "          leaves for all traces of size smaller than or equal to <int>.\n\n";
  Printf.printf "      -with_por [compr|red] : Uses Partial Order Reductions techniques to significantly\n";
  Printf.printf "          improve performance. It is possible to choose a specific POR technique (compressed\n";
  Printf.printf "          or reduced semantics). Without extra argument, -with_por option will enable the best\n";
  Printf.printf "          POR tehnique (i.e., reduced semantics).\n";
  Printf.printf "          Note : This option automatically activates the option '-no_comm'.\n";
  Printf.printf "          WARNING : This option should only be used for action-determinate processes.\n\n";
  Printf.printf "      -no_comm : Does not consider the internal communication in the trace equivalence.\n";
  Printf.printf "          WARNING : This option should not be used in presence of private channel.\n\n";
  Printf.printf "      -no_erase : Does not consider a slight optimisation that consists of removing\n";
  Printf.printf "          symbolic processes with the same process during the execution of the algirithm.\n";
  Printf.printf "          Note : This option is automatically activated when -unfold is used.\n\n";
  Printf.printf "      -unfold : Use the glutton strategy that consists of unfolding all symbolic traces\n";
  Printf.printf "          and apply the symbolic equivalence decision procedure for each of them.\n\n";
  Printf.printf "      -verbose [<int>] : Display some statistics on the matrices generated at the.\n";
  Printf.printf "          end of the execution. When an integer <int> is given, the statistics are displayed\n";
  Printf.printf "          every <int> matrices generated.\n\n"
  
(************************
***       Parsing     ***
*************************)

let parse_file path = 
  Parser.Parser_function.initialise_environment ();
  Parser.Parser_function.equivalence_request := [];
    
  Printf.printf "Opening file %s\n" path;
  
  let channel_in = open_in path in
  let lexbuf = Lexing.from_channel channel_in in
  
  Debug.display_debug "Start loop";
  
  let _ = 
    try
      while true do
        Debug.display_debug "Start parsing a declaration";
        Parser.Parser_function.parse_one_declaration (Parser.Grammar.main Parser.Lexer.token lexbuf)
      done
    with 
      | Failure msg -> Printf.printf "%s\n" msg; exit 0
      | End_of_file -> () in
      
      
  close_in channel_in
  
(**************************************
***    Decide trace equivalence     ***
***************************************)

let display_channel_and_stdout channel str =
  Printf.printf "%s" str;
  Printf.fprintf channel "%s" str

let decide_trace_equivalence process1 process2 =
  let channel = Trace_equivalence.Statistic.reset_statistic () in

  display_channel_and_stdout channel "------------\n";
  display_channel_and_stdout channel "Equivalence between the two following processes:\n\n";
  
  
  display_channel_and_stdout channel "Process 1 =\n";
  display_channel_and_stdout channel (Standard_library.Process.display_process process1);
  
  display_channel_and_stdout channel "\n\nProcess2 =\n";
  display_channel_and_stdout channel (Standard_library.Process.display_process process2);
  
  display_channel_and_stdout channel "\n\nStarting the decision procedure...\n";
  flush_all ();
  
  let begin_time = Sys.time () in
  let result = Trace_equivalence.Algorithm.decide_trace_equivalence process1 process2 in
  let end_time = Sys.time () in
  let time_spent = end_time -. begin_time in
  display_channel_and_stdout channel (Printf.sprintf "Result : The trace equivalence is %b\n" result);
  display_channel_and_stdout channel (Printf.sprintf "Result obtained in %f seconds\n" time_spent);
  Trace_equivalence.Statistic.display_statistic ();
  flush_all ();
  close_out channel
  
(****************************************************
***    Decide trace equivalence w.r.t. length     ***
*****************************************************)

let decide_trace_equivalence_length process1 process2 =
  let channel = Trace_equivalence.Statistic.reset_statistic () in
  
  display_channel_and_stdout channel "------------\n";
  display_channel_and_stdout channel "Trace equivalence w.r.t. length between the two following processes:\n\n";
  
  display_channel_and_stdout channel "Process 1 =\n";
  display_channel_and_stdout channel (Standard_library.Process.display_process process1);
  
  display_channel_and_stdout channel "\n\nProcess2 =\n";
  display_channel_and_stdout channel (Standard_library.Process.display_process process2);
  
  display_channel_and_stdout channel "\n\nStarting the decision procedure...\n";
  flush_all ();
  
  let begin_time = Sys.time () in
  let result = Length_equivalence.Algorithm.decide_trace_equivalence process1 process2 in
  let end_time = Sys.time () in
  let time_spent = end_time -. begin_time in
  display_channel_and_stdout channel (Printf.sprintf "Result : The trace equivalence w.r.t. length is %b\n" result);
  display_channel_and_stdout channel (Printf.sprintf "Result obtained in %f seconds\n" time_spent);
  Trace_equivalence.Statistic.display_statistic ();
  flush_all ();
  close_out channel
  
(************************
***        Main       ***
*************************)  
  
let _ = 			
  let path = ref "" in
  let arret = ref false in
  let i = ref 1 in

  while !i < Array.length Sys.argv && not !arret do
    match (Sys.argv).(!i) with
      | "-with_por" -> 
          if (Sys.argv).(!i+1) = "compr"
	  then begin
	      Trace_equivalence.Algorithm.option_compr := true;
	      i := !i + 2;
	    end else
	    begin
	      Trace_equivalence.Algorithm.option_compr := true;
	      Trace_equivalence.Algorithm.option_red := true;
	      i := !i + 1;
	    end
      | "-no_comm" -> Trace_equivalence.Algorithm.option_internal_communication := false;
		      i := !i + 1
      | "-unfold" -> 
         Trace_equivalence.Algorithm.option_erase_double := false;
         Trace_equivalence.Algorithm.option_alternating_strategy := false;
          i := !i + 1
      | "-no_erase" -> Trace_equivalence.Algorithm.option_erase_double := false;
          i := !i + 1
      | "-debug" when not (!i+1 = (Array.length Sys.argv)) -> 
          if (Sys.argv).(!i+1) = "none"
          then Debug.initialise_debugging Debug.None
          else if (Sys.argv).(!i+1) = "high"
          then Debug.initialise_debugging Debug.High
          else if (Sys.argv).(!i+1) = "low"
          then Debug.initialise_debugging Debug.Low
          else arret := true;
	  i := !i + 2
      | "-verbose" -> 
          begin try 
            Trace_equivalence.Statistic.initialise_statistic (Trace_equivalence.Statistic.Periodic (int_of_string (Sys.argv).(!i+1)));
            i := !i + 2
          with _ ->
            Trace_equivalence.Statistic.initialise_statistic Trace_equivalence.Statistic.Final;
            i := !i + 1
          end
      | "-display" when not (!i+1 = (Array.length Sys.argv)) -> 
          if (Sys.argv).(!i+1) = "size"
          then Trace_equivalence.Statistic.initialise_display Trace_equivalence.Statistic.Per_size
          else if (Sys.argv).(!i+1) = "step"
          then Trace_equivalence.Statistic.initialise_display Trace_equivalence.Statistic.Per_step
          else arret := true;
	  i := !i + 2
      | "-log" when not (!i+1 = (Array.length Sys.argv)) -> 
          begin try 
            Trace_equivalence.Statistic.initialise_log (int_of_string (Sys.argv).(!i+1))
          with _ -> arret := true
          end;
	  i := !i + 2
      | str_path -> 
          if !i = Array.length Sys.argv - 1
          then path := str_path
          else arret := true;
	  i := !i + 1
  done;
  
  if Array.length Sys.argv <= 1
  then arret := true;
	
  if !arret || !path = ""
  then print_help ()
  else 
    begin
      parse_file !path;
     
      (* Trace equivalence *)
      List.iter (fun (p1,p2) -> decide_trace_equivalence p1 p2) !Parser.Parser_function.equivalence_request;
      
      (* Complete the length functions *)
      Length_equivalence.Length.complete_length_functions ();
      
      (* Trace equivalence w.r.t. length *)
      List.iter (fun (p1,p2) -> decide_trace_equivalence_length p1 p2) !Parser.Parser_function.equivalence_length_request
    end
