
let is_ml_mli file = 
  let size = String.length file in
  if (String.sub file (size-3) 3 = ".ml") || (String.sub file (size-4) 4 = ".mli")
  then true
  else false


let is_mll_mly file = 
  let size = String.length file in
  if String.sub file (size-4) 4 = ".mll" || String.sub file (size-4) 4 = ".mly"
  then true
  else false  
  
let authorise_dir = [
  "parser";
  "length_equivalence";
  "standard_library";
  "trace_equivalence"
  ]
  
let rec get_all_adress curdir =
  let vect = Sys.readdir curdir in
  
  for i = 0 to (Array.length vect) -1 do
    let new_adress = Printf.sprintf "%s/%s" curdir vect.(i) in
    if Sys.is_directory new_adress && List.exists (fun t -> t = vect.(i)) authorise_dir
    then 
      begin
        let cmd = Printf.sprintf "mkdir Extract/%s" new_adress in
        Printf.printf "%s\n" cmd;
        ignore (Sys.command cmd);
        get_all_adress new_adress
      end;
      
    if is_ml_mli new_adress 
    then 
      begin
        let cmd = Printf.sprintf "cat Source/licence.txt %s >> Extract/%s" new_adress new_adress in
        Printf.printf "%s\n" cmd;
        ignore (Sys.command cmd)
      end;

    if vect.(i) = "Makefile" || is_mll_mly new_adress
    then 
      begin
        let cmd = Printf.sprintf "cp %s Extract/%s" new_adress new_adress in
        Printf.printf "%s\n" cmd;
        ignore (Sys.command cmd)
      end;
  done

let _ = 
  ignore (Sys.command "rm -rf Extract");
  ignore (Sys.command "mkdir Extract");
  ignore (Sys.command "mkdir Extract/Source");
  ignore (Sys.command "mkdir Extract/Example");
  ignore (Sys.command "cp Example/*.txt Extract/Example");
  ignore (Sys.command "cp README Extract/README");
  ignore (Sys.command "cp Documentation.pdf Extract/Documentation.pdf");
  get_all_adress "Source"
