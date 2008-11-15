
open Printf
open ExtList
open ExtString

module G = Gencode.Make(Gen_OCaml)
module PP = Gencode.Prettyprint

let (|>) x f = f x
let (@@) f x = f x

let file = ref None
let output = ref None
let generators = ref None
let dump_decls = ref false

let arg_spec =
  Arg.align
    [
      "-o", Arg.String (fun f -> output := Some f), "FILE Set output file.";
      "-g", Arg.String (fun gs -> generators := Some (String.nsplit gs ",")),
        "LIST Generators to use (comma-separated).";
      "--debug", Arg.Set dump_decls, " Dump message definitions."
    ]

let usage_msg =
  sprintf
    "\nUsage: extprotc [OPTIONS] <file>\n\n\
     Known generators:\n\n\
     %s\n\n\
     Options:\n" @@
    String.concat "\n" @@
      List.map
        (fun (lang, gens) -> sprintf "  %s: %s" lang @@ String.join ", " gens)
        [
          "OCaml", G.generators;
        ]

let print_field bindings const fname mutabl ty =
  let reduced = Ptypes.type_expr ty |> Gencode.beta_reduce_texpr bindings in
    Format.fprintf Format.err_formatter "%!%S - %S mutable: %s@.%a@.@."
      const fname (string_of_bool mutabl) PP.inspect_reduced_type_expr reduced

let inspect_decls decls bindings =
  prerr_newline ();
  prerr_endline (String.make 60 '*');
  prerr_newline ();
  List.iter
    (function
         Ptypes.Message_decl (name, mexpr, _) ->
           Format.fprintf Format.err_formatter "Message %S:@.@." name;
           Gencode.iter_message (print_field bindings) mexpr;
           Format.fprintf Format.err_formatter "---@.@."
       | Ptypes.Type_decl _ -> ())
    decls

let () =
  Arg.parse arg_spec (fun fname -> file := Some fname) usage_msg;
  Option.may
    (fun file ->
       let output = match !output with
           None -> Filename.chop_extension file ^ ".ml"
         | Some f -> f in
       let och = open_out output in
       let decls = Parser.print_synerr Parser.parse_file file in
         begin
           match Ptypes.check_declarations decls with
               [] -> G.generate_code ?generators:!generators decls |> output_string och
             | errors -> Ptypes.print_errors stderr errors
         end;
         if !dump_decls then inspect_decls decls (Gencode.collect_bindings decls))
    !file
