open Printf
module E = Extprot
module PP = E.Pretty_print

let verbose = ref false

let arg_spec =
  Arg.align
    [
      "-v", Arg.Set verbose, " Set verbose mode.";
    ]

let () =
  Arg.parse arg_spec ignore
    "\nUsage: pretty_print [OPTIONS] < <file>\n\nOptions:";
  try
    let io = IO.input_channel stdin in
      try
        while true do
          E.Inspect_msg.inspect ~verbose:!verbose Format.std_formatter io;
          Format.print_newline ()
        done;
      with IO.No_more_input -> ()
  with E.Error.Extprot_error (err, loc) ->
    printf "Extprot_error (%s)\n" (PP.pp E.Error.pp_extprot_error (err, loc))
