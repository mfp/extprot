
open Printf
open Extprot
open ExtList

let decls = Parser.print_synerr Parser.parse_file "tst.proto"
let bindings = Gencode.collect_bindings decls

module G = Gen_OCaml
module PP = Gencode.Prettyprint

let (|>) x f = f x

let print_field const fname mutabl ty =
  let reduced = Ptypes.type_expr ty |> Gencode.beta_reduce_texpr bindings in
    Format.fprintf Format.std_formatter "%!%S - %S mutable: %s\n @[<1>%a@]\n\n%!"
      const fname (string_of_bool mutabl) PP.pp_reduced_type_expr reduced

let () =
  decls |> List.filter_map (fun decl -> G.generate_container bindings decl)
    |> G.generate_code |> print_endline;

  print_newline ();
  print_endline (String.make 60 '*');
  print_newline ();
  List.iter
    (function
         Ptypes.Message_decl (name, mexpr) ->
           Format.fprintf Format.std_formatter "Message %S:\n" name;
           Gencode.iter_message print_field mexpr;
           Format.fprintf Format.std_formatter "\n---\n\n"
       | Ptypes.Type_decl (_, _, _) -> ())
    decls;
  ()


