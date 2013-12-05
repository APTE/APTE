(********************
***      Help     ***
*********************)

let print_help () = 
  Printf.printf "Name : APTE\n";
  Printf.printf "   Algorithm for Proving Trace Equivalence\n\n";
  Printf.printf "Version 0.3alpha\n\n";
  Printf.printf "Synopsis :\n";
  Printf.printf "      apte [-debug [high|low|none]] file\n";
  Printf.printf "Options :\n";
  Printf.printf "      -debug [high|low|none] : APTE is programmed with three level of debugging.\n";
  Printf.printf "          The High debugging option checks several invariants of the algorithms.\n";
  Printf.printf "          While this mode provides more guarantee about the result, it makes the\n";
  Printf.printf "          algorithm slower.\n";
  Printf.printf "          The Low debugging option only checks basic invariants. (default) \n";
  Printf.printf "          The None debugging option does not check any invariant. Chose this option\n";
  Printf.printf "          for optimal running time.\n"
  
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

let decide_trace_equivalence process1 process2 =
  Printf.printf "------------\n";
  Printf.printf "Equivalence between the two following processes:\n\n";
  
  Printf.printf "Process 1 =\n";
  print_string (Standard_library.Process.display_process process1);
  
  Printf.printf "\n\nProcess2 =\n";
  print_string (Standard_library.Process.display_process process2);
  
 
  Printf.printf "\n\nStarting the decision procedure...\n";
  flush_all ();
  
  Trace_equivalence.Algorithm.internal_communication := false;
  
  let begin_time = Sys.time () in
  let result = Trace_equivalence.Algorithm.decide_trace_equivalence process1 process2 in
  let end_time = Sys.time () in
  let time_spent = end_time -. begin_time in
  Printf.printf "Result : The trace equivalence is %b\n" result;
  Printf.printf "Result obtained in %f seconds\n" time_spent;
  flush_all ()
  
(****************************************************
***    Decide trace equivalence w.r.t. length     ***
*****************************************************)

let decide_trace_equivalence_length process1 process2 =
  Printf.printf "------------\n";
  Printf.printf "Trace equivalence w.r.t. length between the two following processes:\n\n";
  
  Printf.printf "Process 1 =\n";
  print_string (Standard_library.Process.display_process process1);
  
  Printf.printf "\n\nProcess2 =\n";
  print_string (Standard_library.Process.display_process process2);
  
 
  Printf.printf "\n\nStarting the decision procedure...\n";
  flush_all ();
  
  Length_equivalence.Algorithm.internal_communication := false;
  
  let begin_time = Sys.time () in
  let result = Length_equivalence.Algorithm.decide_trace_equivalence process1 process2 in
  let end_time = Sys.time () in
  let time_spent = end_time -. begin_time in
  Printf.printf "Result : The trace equivalence w.r.t. length is %b\n" result;
  Printf.printf "Result obtained in %f seconds\n" time_spent;
  flush_all ()
  
(************************
***        Main       ***
*************************)  
  
let _ = 			
  let path = ref "" in
  let arret = ref false in
  let i = ref 1 in
  
  while !i < Array.length Sys.argv && not !arret do
    match (Sys.argv).(!i) with
      | "-debug" when not (!i+1 = (Array.length Sys.argv)) -> 
          if (Sys.argv).(!i+1) = "none"
          then Debug.initialise_debugging Debug.None
          else if (Sys.argv).(!i+1) = "high"
          then Debug.initialise_debugging Debug.High
          else Debug.initialise_debugging Debug.Low;
          
	  i := !i + 2
      | str_path -> 
          path := str_path;
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
