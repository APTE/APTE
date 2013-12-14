let version = "0.4beta"

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
        let cmd = Printf.sprintf "mkdir APTE-%s/%s" version new_adress in
        Printf.printf "%s\n" cmd;
        ignore (Sys.command cmd);
        get_all_adress new_adress
      end;
      
    if is_ml_mli new_adress 
    then 
      begin
        let cmd = Printf.sprintf "cat Source/licence.txt %s >> APTE-%s/%s" new_adress version new_adress in
        Printf.printf "%s\n" cmd;
        ignore (Sys.command cmd)
      end;

    if vect.(i) = "Makefile" || is_mll_mly new_adress
    then 
      begin
        let cmd = Printf.sprintf "cp %s APTE-%s/%s" new_adress version new_adress in
        Printf.printf "%s\n" cmd;
        ignore (Sys.command cmd)
      end;
  done
 

let _ = 
  ignore (Sys.command (Printf.sprintf "rm -rf APTE-%s" version));
  ignore (Sys.command (Printf.sprintf "mkdir APTE-%s" version));
  ignore (Sys.command (Printf.sprintf "mkdir APTE-%s/Source" version));
  ignore (Sys.command (Printf.sprintf "mkdir APTE-%s/Example" version));
  ignore (Sys.command (Printf.sprintf "cp Example/*.txt APTE-%s/Example" version));
  ignore (Sys.command (Printf.sprintf "cp README.md APTE-%s/README.md" version));
  ignore (Sys.command (Printf.sprintf "cp LICENSE APTE-%s/LICENSE" version));
  ignore (Sys.command (Printf.sprintf "cp ChangeLog APTE-%s/ChangeLog" version));
  ignore (Sys.command (Printf.sprintf "cp Documentation.pdf APTE-%s/Documentation.pdf" version));
  get_all_adress "Source";
  ignore (Sys.command (Printf.sprintf "zip -r APTE-%s.zip APTE-%s/" version version));
