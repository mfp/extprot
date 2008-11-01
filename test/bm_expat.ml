open Printf

let parse s =
  let p = Expat.parser_create None in
  try
    Expat.parse p s
  with Expat.Expat_error e as exn ->
    printf "Error (%d:%d): %s\n"
      (Expat.get_current_column_number p)
      (Expat.get_current_line_number p)
      (Expat.xml_error_to_string e);
    raise exn

let rounds = ref 10

let arg_spec =
  Arg.align
    [
      "-n", Arg.Set_int rounds, "INT Number of iterations."
    ]

let () =
  Arg.parse arg_spec ignore "\nUsage: bm_expat [OPTIONS]\n\nOptions:" in
  let s = String.concat "\n" ["<doc>"; Std.input_all stdin; "</doc>"] in
  let dts =
    Array.init !rounds
      (fun _ -> let t0 = Unix.gettimeofday () in parse s; Unix.gettimeofday () -. t0)
  in
    printf "Read %d bytes.\n" (String.length s);
    printf "Avg: %8.5fs\n" (Array.fold_left (+.) 0. dts /. float !rounds);
    printf "Min: %8.5fs\n" (Array.fold_left min max_float dts)
