(*******************************
***    Statistic function     ***
********************************)

open Standard_library

type statistic_mode =
  | Final
  | Periodic of int
  | None
  
type display_mode =
  | Global
  | Per_size
  | Per_step 
  
type step_strategy = 
  | POne_SA | POne_SB | POne_SC | POne_SD | POne_SE 
  | PTwo_SA | PTwo_SB | PTwo_SC 
  | Leaf  
  
type matrix_statistic = 
  {
    (* Matrix *)
    nb_matrix : int;
    nb_column : int;
    nb_line : int;
    pourcent_bot : int;
  } 
  
let initial_matrix_statistic =
  {
    (* Matrix *)
    nb_matrix = 0;
    nb_column = 0;
    nb_line = 0;
    pourcent_bot = 0;
  }

type process_statistic = 
  {
    (* Statistic on the trace *)
    size_trace : int;
    mutable nb_trace : int;
    
    (* Statistic on the matrices *)
    stat_per_step : matrix_statistic array
  }

let total_nb_matrix = ref 0

let statistic_DB = ref []

let temporary_buffer = ref []

(** Record functions **)

let record_matrix_function = ref (fun _ _ -> ())

let start_transition_function = ref (fun _ _ -> ())

let end_transition_function = ref (fun _ -> ())

(** Sub Record functions **)

let sub_record_matrix step matrix = 
  let proc_stat = List.hd !temporary_buffer in
  
  let index = 
    match step with
      | POne_SA -> 0
      | POne_SB -> 1
      | POne_SC -> 2
      | POne_SD -> 3
      | POne_SE -> 4
      | PTwo_SA -> 5
      | PTwo_SB -> 6
      | PTwo_SC -> 7
      | Leaf -> 8
    in
    
  let nb_bot = ref 0 in
  let nb_total = ref 0 in
  
  Constraint_system.Matrix.iter (fun csys -> 
    nb_total := !nb_total + 1;
    
    if Constraint_system.is_bottom csys
    then nb_bot := !nb_bot + 1
    else ()
  ) matrix;
  
  let nb_column = Constraint_system.Matrix.get_number_column matrix
  and nb_line = Constraint_system.Matrix.get_number_line matrix
  and pourcent_bot = ((!nb_bot * 100) / !nb_total) in
    
  let old = proc_stat.stat_per_step.(index) in
  
  let new_stat = 
    { 
      nb_matrix = old.nb_matrix + 1;
      nb_column = old.nb_column + nb_column;
      nb_line = old.nb_line + nb_line;
      pourcent_bot = old.pourcent_bot + pourcent_bot;
    }
  in
  
  proc_stat.stat_per_step.(index) <- new_stat

let sub_start_transition list1 list2 =
  
  let rec sub_func size l = match l with
    | [] ->
        let proc_stat =
          {
            size_trace = size;
            nb_trace = 0;
            stat_per_step = Array.make 9 initial_matrix_statistic
          }
        in 
        (proc_stat::l,proc_stat)
    | t::_ when t.size_trace > size -> 
        let proc_stat =
          {
            size_trace = size;
            nb_trace = 0;
            stat_per_step = Array.make 9 initial_matrix_statistic
          }
        in 
        (proc_stat::l,proc_stat)
    | t::_ when t.size_trace = size -> (l,t)
    | t::q -> let (next_l,next_t) = sub_func size q in
        (t::next_l,next_t)
  in
  
  let get_stat_proc size =
    let (next_DB,stat) = sub_func size !statistic_DB in
    statistic_DB := next_DB;
    temporary_buffer := stat :: !temporary_buffer;
    stat.nb_trace <- stat.nb_trace + 1
  in
  
  if list1 = []
  then 
    if list2 = []
    then ()
    else 
      let size = Process.size_trace (List.hd list2) in
      get_stat_proc size
  else
    let size = Process.size_trace (List.hd list1) in
    get_stat_proc size
    
let sub_end_transition () = temporary_buffer := List.tl (!temporary_buffer)

(** Final function **)
  
let record_matrix = !record_matrix_function

let start_transition = !start_transition_function

let end_transition = !end_transition_function

(********************************
***     Display functions     ***
*********************************)

let display_matrix_statistic m_stat nb_trace =
  Printf.sprintf "Nb of matrices = %d; Average nb of matrices per trace = %d; Average nb of columns = %d; Average nb of lines = %d; Density of bot = %d%%\n"
    m_stat.nb_matrix
    (m_stat.nb_matrix / nb_trace)
    (m_stat.nb_column / m_stat.nb_matrix)
    (m_stat.nb_line / m_stat.nb_matrix)
    (m_stat.pourcent_bot / m_stat.nb_matrix)

(** Display per size of traces *)    
let display_per_size () =

  let overall_nb_trace = ref 0
  and overall_m_stat = ref initial_matrix_statistic in

  let rec go_through_step m_tbl = function
    | 9 -> initial_matrix_statistic
    | k -> 
        let m_stat = go_through_step m_tbl (k+1) in
        
        {
          nb_matrix = m_stat.nb_matrix + m_tbl.(k).nb_matrix;
          nb_column = m_stat.nb_column + m_tbl.(k).nb_column;
          nb_line = m_stat.nb_line + m_tbl.(k).nb_line;
          pourcent_bot = m_stat.pourcent_bot + m_tbl.(k).pourcent_bot
        }
  in
  
  let rec go_through_DB = function
    | [] -> ()
    | proc_stat::q -> 
        let m_stat = go_through_step proc_stat.stat_per_step 0 in
        Printf.printf "For traces of size %d: %s" 
          proc_stat.size_trace
          (display_matrix_statistic m_stat proc_stat.nb_trace);
        
        overall_nb_trace := !overall_nb_trace + proc_stat.nb_trace;
        overall_m_stat :=
          {
            nb_matrix = !overall_m_stat.nb_matrix + m_stat.nb_matrix;
            nb_column = !overall_m_stat.nb_column + m_stat.nb_column;
            nb_line = !overall_m_stat.nb_line + m_stat.nb_line;
            pourcent_bot = !overall_m_stat.pourcent_bot + m_stat.pourcent_bot
          };
        
        go_through_DB q
  in
  
  Printf.printf "-------------------------------------------\n";
  Printf.printf "Statistics on matrices per size of traces:\n";
  go_through_DB !statistic_DB;
  Printf.printf "\nOverall statistics: %s\n"
    (display_matrix_statistic !overall_m_stat !overall_nb_trace)
        
(** Display per steps of the strategy *)

let display_steps = function 
  | 0 -> "phase 1 step a"
  | 1 -> "phase 1 step b"
  | 2 -> "phase 1 step c"
  | 3 -> "phase 1 step d"
  | 4 -> "phase 1 step e"
  | 5 -> "phase 2 step a"
  | 6 -> "phase 2 step b"
  | 7 -> "phase 2 step c"
  | 8 -> "leaves"
  | _ -> Debug.internal_error "[statistic.ml > display_steps] There should not be a step index greater than 8" 

let display_per_step () =

  let overall_nb_trace = ref 0
  and overall_m_stat = ref initial_matrix_statistic
  and overall_stat_per_step = Array.make 9 initial_matrix_statistic in
  
  List.iter (fun proc_stat ->
    overall_nb_trace := !overall_nb_trace + proc_stat.nb_trace;
    
    for i = 0 to 8 do
      let m_stat = proc_stat.stat_per_step.(i) in
      
      overall_stat_per_step.(i) <-
        {
          nb_matrix = m_stat.nb_matrix + overall_stat_per_step.(i).nb_matrix;
          nb_column = m_stat.nb_column + overall_stat_per_step.(i).nb_column;
          nb_line = m_stat.nb_line + overall_stat_per_step.(i).nb_line;
          pourcent_bot = m_stat.pourcent_bot + overall_stat_per_step.(i).pourcent_bot;
        }
    done
  ) !statistic_DB;
  
  for i = 0 to 8 do
    overall_m_stat :=
      {
        nb_matrix = !overall_m_stat.nb_matrix + overall_stat_per_step.(i).nb_matrix;
        nb_column = !overall_m_stat.nb_column + overall_stat_per_step.(i).nb_column;
        nb_line = !overall_m_stat.nb_line + overall_stat_per_step.(i).nb_line;
        pourcent_bot = !overall_m_stat.pourcent_bot + overall_stat_per_step.(i).pourcent_bot;
      }
  done;
  
  Printf.printf "-------------------------------------------\n";
  Printf.printf "Statistics on matrices per step of the strategy:\n";
  
  for i = 0 to 8 do
    Printf.printf "For matrices in %s: %s" 
      (display_steps i)
      (display_matrix_statistic overall_stat_per_step.(i) !overall_nb_trace)
  done;
  
  Printf.printf "\nOverall statistics: %s\n"
    (display_matrix_statistic !overall_m_stat !overall_nb_trace)
  
(** Display per steps of the strategy *)    
    
let display_global () =

  let overall_nb_trace = ref 0
  and overall_m_stat = ref initial_matrix_statistic
  and overall_stat_per_step = Array.make 9 initial_matrix_statistic in
  
  List.iter (fun proc_stat ->
    overall_nb_trace := !overall_nb_trace + proc_stat.nb_trace;
    
    for i = 0 to 8 do
      let m_stat = proc_stat.stat_per_step.(i) in
      
      overall_stat_per_step.(i) <-
        {
          nb_matrix = m_stat.nb_matrix + overall_stat_per_step.(i).nb_matrix;
          nb_column = m_stat.nb_column + overall_stat_per_step.(i).nb_column;
          nb_line = m_stat.nb_line + overall_stat_per_step.(i).nb_line;
          pourcent_bot = m_stat.pourcent_bot + overall_stat_per_step.(i).pourcent_bot;
        }
    done
  ) !statistic_DB;
  
  for i = 0 to 8 do
    overall_m_stat :=
      {
        nb_matrix = !overall_m_stat.nb_matrix + overall_stat_per_step.(i).nb_matrix;
        nb_column = !overall_m_stat.nb_column + overall_stat_per_step.(i).nb_column;
        nb_line = !overall_m_stat.nb_line + overall_stat_per_step.(i).nb_line;
        pourcent_bot = !overall_m_stat.pourcent_bot + overall_stat_per_step.(i).pourcent_bot;
      }
  done;
  
  Printf.printf "-------------------------------------------\n";
  Printf.printf "Overall statistics: %s\n"
    (display_matrix_statistic !overall_m_stat !overall_nb_trace)
      
    
let display_statistic_function = ref display_global

let initialise_display = function
  | Global -> display_statistic_function := display_global
  | Per_size -> display_statistic_function := display_per_size
  | Per_step -> display_statistic_function := display_per_step
    
let display_statistic () = !display_statistic_function ()
  
(** Initialise statistic *)

let initialise_statistic = function
  | Final -> 
      record_matrix_function := sub_record_matrix;
      start_transition_function := sub_start_transition;
      end_transition_function := sub_end_transition
  | Periodic(n) ->
      record_matrix_function := 
        (fun s m -> 
          sub_record_matrix s m;
          total_nb_matrix := !total_nb_matrix + 1;
          if (!total_nb_matrix / n)*n = !total_nb_matrix
          then display_statistic ()
          else ()
        );
      start_transition_function := sub_start_transition;
      end_transition_function := sub_end_transition
  | None ->
      record_matrix_function := fun _ _ -> ();
      start_transition_function := fun _ _ -> ();
      end_transition_function := fun () -> ()


