
open Extprot

let decls = Parser.print_synerr Parser.parse_file "tst.proto"

let bindings = Gencode.collect_bindings decls

module G = Gen_OCaml

let pr = Camlp4.PreCast.Printers.OCaml.print_implem

let pr_cont decl = match G.generate_container bindings decl with
    Some c -> (match c.G.c_types with
                   Some stritem -> pr stritem
                 | _ -> ())
  | _ -> ()

let () =
  List.iter pr_cont decls

