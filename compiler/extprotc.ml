
open Printf
open Extprot_gen
open ExtList
open ExtString

module G = Gencode.Make(Gen_OCaml)

let (|>) x f = f x
let (@@) f x = f x

let file = ref None
let output = ref None
let generators = ref None

let arg_spec =
  Arg.align
    [
      "-o", Arg.String (fun f -> output := Some f), "FILE Set output file.";
      "-g", Arg.String (fun gs -> generators := Some (String.nsplit gs ",")),
        "LIST Generators to use (comma-separated).";
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

let () =
  Arg.parse arg_spec (fun fname -> file := Some fname) usage_msg;
  Option.may
    (fun file ->
       let output = match !output with
           None -> Filename.chop_extension file ^ ".ml"
         | Some f -> f in
       let och = open_out output in
       let decls = Parser.print_synerr Parser.parse_file file in
       match Ptypes.check_declarations decls with
           [] -> G.generate_code ?generators:!generators decls |> output_string och
         | errors -> Ptypes.print_errors stderr errors)
    !file

module PP = Gencode.Prettyprint

let print_field bindings const fname mutabl ty =
  let reduced = Ptypes.type_expr ty |> Gencode.beta_reduce_texpr bindings in
    Format.fprintf Format.std_formatter "%!%S - %S mutable: %s\n @[<1>%a@]\n\n%!"
      const fname (string_of_bool mutabl) PP.pp_reduced_type_expr reduced

let inspect_decls decls bindings =
  print_newline ();
  print_endline (String.make 60 '*');
  print_newline ();
  List.iter
    (function
         Ptypes.Message_decl (name, mexpr) ->
           Format.fprintf Format.std_formatter "Message %S:\n" name;
           Gencode.iter_message (print_field bindings) mexpr;
           Format.fprintf Format.std_formatter "\n---\n\n"
       | Ptypes.Type_decl (_, _, _) -> ())
    decls;


