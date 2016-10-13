(*******************************
***    Statistic function     ***
********************************)

open Standard_library

(** Option *)

let option_size_trace_log = ref 0

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

(** Type for gathering statistics *)

type matrix_statistic =
  {
    (* Matrix *)
    nb_matrix : int;

    nb_column : int;
    nb_line : int;
    pourcent_bot : int;

    max_column : int;
    max_line : int;
    max_pourcent_bot : int
  }

type process_statistic =
  {
    (* Statistic on the trace *)
    size_trace : int;
    mutable nb_trace : int;

    (* Statistic on the matrices *)
    stat_per_step : matrix_statistic array
  }

type temporary_statistic =
  {
    channel : out_channel;
    dir_for_sons : string;
    mutable nb_sons : int;

    local_matrix_stat : matrix_statistic array;
    size_trace_t : int;

    current_proc_stat : process_statistic
  }

(** Mains parameters *)


let record_active = ref false

let total_nb_matrix = ref 0

let number_of_input = ref 0

let statistic_DB = ref []

let temporary_buffer = ref []

(** Initials values *)

let initial_matrix_statistic =
  {
    (* Matrix *)
    nb_matrix = 0;

    nb_column = 0;
    nb_line = 0;
    pourcent_bot = 0;

    max_column = 0;
    max_line = 0;
    max_pourcent_bot = 0
  }

let initial_process_statistic =
  {
    size_trace = 0;
    nb_trace = 0;
    stat_per_step = Array.make 0 initial_matrix_statistic
  }

let log_dir = ref (
  let executable_name = Sys.executable_name in
  let index_slash = String.rindex executable_name '/' in
  let main_dir = String.sub executable_name 0 (index_slash+1) in
  (main_dir^"log"))

let sys_command =
  Printf.ksprintf
    (fun s ->
       let ret = Sys.command s in
         if ret <> 0 then begin
           Printf.eprintf "Command %S returned %d. Exiting...\n" s ret ;
           exit ret
         end)

let () =
	(* In that case, we choose another log folder to avoid removing
           important files *)
	Random.self_init ();
	let rdm = Random.int 1000000000 in
	log_dir := (!log_dir)^(Printf.sprintf "%d" rdm);
	sys_command "mkdir %s" !log_dir

let reset_statistic () =
  statistic_DB := [];
  number_of_input := !number_of_input + 1;

  let main_out_channel = open_out (Printf.sprintf "%s/input_%d.log" !log_dir !number_of_input) in
  let dir_for_matrix = Printf.sprintf "%s/input_%d_matrices" !log_dir !number_of_input in
  let () = sys_command "mkdir %s" dir_for_matrix in
  let initial =
    {
      channel = main_out_channel;
      dir_for_sons = dir_for_matrix;
      nb_sons = 0;
      local_matrix_stat = Array.make 0 initial_matrix_statistic;
      size_trace_t = 0;
      current_proc_stat = initial_process_statistic
    }
  in
  temporary_buffer := [initial];
  main_out_channel

(** Auxilliary functions **)

let add_matrix_statistic m1 m2 =
  {
    nb_matrix = m1.nb_matrix + m2.nb_matrix;
    nb_column = m1.nb_column + m2.nb_column;
    nb_line = m1.nb_line + m2.nb_line;
    pourcent_bot = m1.pourcent_bot + m2.pourcent_bot;
    max_column = max m1.max_column m2.max_column;
    max_line = max m1.max_line m2.max_line;
    max_pourcent_bot = max m1.max_pourcent_bot m2.max_pourcent_bot
  }

(********************************
***     Display functions     ***
*********************************)

let divide_2_decimal a b =
  (float_of_int ((a * 100) / b)) /. 100.

let display_matrix_statistic m_stat nb_trace =
  if m_stat.nb_matrix  = 0
  then
    Printf.sprintf "No matrix\n"
  else
  if nb_trace = 0
  then
    Printf.sprintf "Number of matrices = %d;\n        Average (resp. maximal) number of columns per matrices = %g (resp. %d);\n        Average (resp. maximal) number of lines in matrices = %g (resp. %d);\n        Average (resp. maximal) density of bot in matrices= %g%% (resp. %d%%)\n"
      m_stat.nb_matrix
      (divide_2_decimal m_stat.nb_column m_stat.nb_matrix)
      m_stat.max_column
      (divide_2_decimal m_stat.nb_line m_stat.nb_matrix)
      m_stat.max_line
      (divide_2_decimal m_stat.pourcent_bot m_stat.nb_matrix)
      m_stat.max_pourcent_bot
  else
    Printf.sprintf "Number of traces = %d; Number of matrices = %d; Average number of matrices per trace = %g;\n        Average (resp. maximal) number of columns per matrices = %g (resp. %d);\n        Average (resp. maximal) number of lines in matrices = %g (resp. %d);\n        Average (resp. maximal) density of bot in matrices= %g%% (resp. %d%%)\n"
      nb_trace
      m_stat.nb_matrix
      (divide_2_decimal m_stat.nb_matrix nb_trace)
      (divide_2_decimal m_stat.nb_column m_stat.nb_matrix)
      m_stat.max_column
      (divide_2_decimal m_stat.nb_line m_stat.nb_matrix)
      m_stat.max_line
      (divide_2_decimal m_stat.pourcent_bot m_stat.nb_matrix)
      m_stat.max_pourcent_bot

(** Display per size of traces *)
let display_per_size () =

  let overall_nb_trace = ref 0
  and overall_m_stat = ref initial_matrix_statistic in

  let rec go_through_step m_tbl = function
    | 9 -> initial_matrix_statistic
    | k ->
        let m_stat = go_through_step m_tbl (k+1) in
        add_matrix_statistic m_stat m_tbl.(k)
  in

  let rec go_through_DB = function
    | [] -> ()
    | proc_stat::q ->
        let m_stat = go_through_step proc_stat.stat_per_step 0 in
        Printf.printf "For traces of size %d: %s"
          proc_stat.size_trace
          (display_matrix_statistic m_stat proc_stat.nb_trace);

        overall_nb_trace := !overall_nb_trace + proc_stat.nb_trace;
        overall_m_stat := add_matrix_statistic !overall_m_stat m_stat;

        go_through_DB q
  in

  Printf.printf "-------------------------------------------\n";
  Printf.printf "Statistics on matrices per size of traces:\n";
  go_through_DB !statistic_DB;
  Printf.printf "\nOverall statistics: %s\n"
    (display_matrix_statistic !overall_m_stat !overall_nb_trace);

  flush_all ()

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

      overall_stat_per_step.(i) <- add_matrix_statistic m_stat overall_stat_per_step.(i)
    done
  ) !statistic_DB;

  for i = 0 to 8 do
    overall_m_stat := add_matrix_statistic !overall_m_stat overall_stat_per_step.(i)
  done;

  Printf.printf "-------------------------------------------\n";
  Printf.printf "Statistics on matrices per step of the strategy:\n";

  for i = 0 to 8 do
    Printf.printf "For matrices in %s: %s"
      (display_steps i)
      (display_matrix_statistic overall_stat_per_step.(i) !overall_nb_trace)
  done;

  Printf.printf "\nOverall statistics: %s\n"
    (display_matrix_statistic !overall_m_stat !overall_nb_trace);

  flush_all ()

(** Display global of the strategy *)

let display_global () =

  let overall_nb_trace = ref 0
  and overall_m_stat = ref initial_matrix_statistic
  and overall_stat_per_step = Array.make 9 initial_matrix_statistic in

  List.iter (fun proc_stat ->
    overall_nb_trace := !overall_nb_trace + proc_stat.nb_trace;

    for i = 0 to 8 do
      let m_stat = proc_stat.stat_per_step.(i) in

      overall_stat_per_step.(i) <- add_matrix_statistic m_stat overall_stat_per_step.(i)
    done
  ) !statistic_DB;

  for i = 0 to 8 do
    overall_m_stat := add_matrix_statistic !overall_m_stat overall_stat_per_step.(i)
  done;

  Printf.printf "-------------------------------------------\n";
  Printf.printf "Nb trace %d\n" !overall_nb_trace;
  Printf.printf "Overall statistics: %s\n"
    (display_matrix_statistic !overall_m_stat !overall_nb_trace)


let display_statistic_function = ref (fun () -> if !record_active then display_global () else ())

(** Sub Record functions **)

let sub_record_matrix step matrix =
  if not (Constraint_system.Matrix.is_empty matrix)
  then begin
    let temp_stat = List.hd !temporary_buffer in
    total_nb_matrix := !total_nb_matrix + 1;

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

    if index = 8 && temp_stat.size_trace_t <= !option_size_trace_log
    then Printf.fprintf temp_stat.channel "%s\n" (Constraint_system.Matrix.display matrix);

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

    let old_local = temp_stat.local_matrix_stat.(index) in
    let old_general = temp_stat.current_proc_stat.stat_per_step.(index) in

    let new_local_stat =
      {
        nb_matrix = old_local.nb_matrix + 1;
        nb_column = old_local.nb_column + nb_column;
        nb_line = old_local.nb_line + nb_line;
        pourcent_bot = old_local.pourcent_bot + pourcent_bot;

        max_column = max old_local.max_column nb_column;
        max_line = max old_local.max_line nb_line;
        max_pourcent_bot = max old_local.max_pourcent_bot pourcent_bot
      }
    in

    let new_general_stat =
      {
        nb_matrix = old_general.nb_matrix + 1;
        nb_column = old_general.nb_column + nb_column;
        nb_line = old_general.nb_line + nb_line;
        pourcent_bot = old_general.pourcent_bot + pourcent_bot;

        max_column = max old_general.max_column nb_column;
        max_line = max old_general.max_line nb_line;
        max_pourcent_bot = max old_general.max_pourcent_bot pourcent_bot
      }
    in

    temp_stat.local_matrix_stat.(index) <- new_local_stat;
    temp_stat.current_proc_stat.stat_per_step.(index) <- new_general_stat
  end

let search_in_DB size =

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

  let new_DB,stat = sub_func size !statistic_DB in
  statistic_DB := new_DB;
  stat

let sub_start_transition list1 list2 =
  let size =
    if list1 = []
    then
      if list2 = []
      then Debug.internal_error "[statistic.ml] There should not be two empty list"
      else Process.size_trace (List.hd list2)
    else Process.size_trace (List.hd list1)
  in

  if size <= !option_size_trace_log
  then
  begin
    let temp_stat = List.hd !temporary_buffer in
    temp_stat.nb_sons <- temp_stat.nb_sons + 1;
    let new_dir = Printf.sprintf "%s/Nb_%d_[size_%d]" temp_stat.dir_for_sons temp_stat.nb_sons size in
    let () = sys_command "mkdir %s" new_dir in
    let main_out_channel = open_out (Printf.sprintf "%s/matrices.log" new_dir) in

    Printf.fprintf main_out_channel "Left set of symbolic process (number: %d):\n\n" (List.length list1);
    List.iter (fun symb_proc -> Printf.fprintf main_out_channel "%s" (Process.display_trace_no_unif symb_proc)) list1;
    Printf.fprintf main_out_channel "\n\n----------------------\nRight set of symbolic process (number: %d):\n\n" (List.length list1);
    List.iter (fun symb_proc -> Printf.fprintf main_out_channel "%s" (Process.display_trace_no_unif symb_proc)) list2;
    Printf.fprintf main_out_channel "\n\n\n-----------------------------\n The Matrices on leaves\n-----------------------------\n";

    let new_temp_stat =
      {
        channel = main_out_channel;
        dir_for_sons = new_dir;
        nb_sons = 0;
        local_matrix_stat = Array.make 9 initial_matrix_statistic;
        size_trace_t = size;
        current_proc_stat = search_in_DB size
      }
    in

    temporary_buffer := new_temp_stat::!temporary_buffer
  end
  else
    let new_temp_stat =
      {
        channel = stdout;
        dir_for_sons = "";
        nb_sons = 0;
        local_matrix_stat = Array.make 9 initial_matrix_statistic;
        size_trace_t = size;
        current_proc_stat = search_in_DB size
      }
    in

    temporary_buffer := new_temp_stat::!temporary_buffer


let sub_end_transition () =


  let temp_stat = List.hd !temporary_buffer in

  if temp_stat.size_trace_t <= !option_size_trace_log
  then
  begin
    Printf.fprintf temp_stat.channel "\n\n-------------------------------------------\n";
    Printf.fprintf temp_stat.channel "Statistics on matrices per step of the strategy:\n";

    for i = 0 to 8 do
      Printf.fprintf temp_stat.channel "For matrices in %s: %s"
        (display_steps i)
        (display_matrix_statistic temp_stat.local_matrix_stat.(i) 0)
    done;

    close_out temp_stat.channel
  end;

  temporary_buffer := List.tl (!temporary_buffer)

(** Record functions **)

let record_matrix_function = ref (fun _ _ -> ())

let start_transition_function = ref (fun _ _ -> ())

let end_transition_function = ref (fun _ -> ())

(** Final function **)

let record_matrix s m = !record_matrix_function s m

let start_transition l1 l2 = !start_transition_function l1 l2

let end_transition () = !end_transition_function ()

let initialise_display = function
  | Global -> display_statistic_function := fun () -> if !record_active then display_global () else ()
  | Per_size -> display_statistic_function := fun () -> if !record_active then display_per_size () else ()
  | Per_step -> display_statistic_function := fun () -> if !record_active then display_per_step () else ()

let display_statistic () = !display_statistic_function ()


(** Initialise statistic *)

let initialise_log size_trace =
  option_size_trace_log := size_trace;
  record_matrix_function := sub_record_matrix;
  start_transition_function := sub_start_transition;
  end_transition_function := sub_end_transition

let initialise_statistic = function
  | Final ->
      record_active := true;
      record_matrix_function := sub_record_matrix;
      start_transition_function := sub_start_transition;
      end_transition_function := sub_end_transition
  | Periodic(n) ->
      record_active := true;
      record_matrix_function :=
        (fun s m ->
          sub_record_matrix s m;
          if (!total_nb_matrix / n)*n = !total_nb_matrix
          then display_statistic ()
          else ()
        );
      start_transition_function := sub_start_transition;
      end_transition_function := sub_end_transition
  | None ->
      record_active := false;
      record_matrix_function := fun _ _ -> ();
      start_transition_function := fun _ _ -> ();
      end_transition_function := fun () -> ()
