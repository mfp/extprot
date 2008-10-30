
open Printf
open Extprot_gen
open ExtList

module G = Gencode.Make(Gen_OCaml)

let (|>) x f = f x

let file = ref None

let arg_spec =
  Arg.align
    [
    ]

let () =
  Arg.parse arg_spec (fun fname -> file := Some fname) "Usage: extprotc <file>";
  Option.may
    (fun file ->
       let decls = Parser.print_synerr Parser.parse_file file in
       (* let bindings = Gencode.collect_bindings decls in *)
         G.generate_code decls |> print_endline)
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


