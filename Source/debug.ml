(*******************************
***    Debuging function     ***
********************************)

exception Internal_error

type debug_mode = 
  | High
  | Low
  | None

let internal_error msg = 
  Printf.printf "Internal error : %s\nPlease report the bug to cheval@lsv.ens-cachan.fr with the input file and output\n" msg;
  raise Internal_error
  
let high_debug_function = ref (fun _ -> ())

let low_debug_function = ref (fun f -> f ())

let initialise_debugging = function
  | High -> 
      high_debug_function := fun f -> f ();
      low_debug_function := fun f -> f ()
  | Low ->
      high_debug_function := fun _ -> ();
      low_debug_function := fun f -> f ()
  | None ->
      high_debug_function := fun _ -> ();
      low_debug_function := fun _ -> ()

  
let high_debugging f = !high_debug_function f

let low_debugging f = !low_debug_function f
(*
let display_debug str =
  Printf.printf "%s\n" str;
  flush_all ()*)
  
let display_debug _ = ()
