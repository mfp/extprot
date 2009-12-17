
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
let dump_reduced = ref false
let dump_concrete = ref false
let width = ref 100

let arg_spec =
  Arg.align
    [
      "-o", Arg.String (fun f -> output := Some f), "FILE Set output file.";
      "-g", Arg.String (fun gs -> generators := Some (String.nsplit gs ",")),
        "LIST Generators to use (comma-separated).";
      "-w", Arg.Set_int width,
        sprintf "N Set width to N characters in generated code (default: %d)." !width;
      "--debug-reduced", Arg.Set dump_reduced, " Dump beta reduced message definitions.";
      "--debug-concrete", Arg.Set dump_concrete, " Dump concrete message definitions.";
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

let print_header ?(sub = '=') fmt =
  kprintf
    (fun s ->
       Format.fprintf Format.err_formatter "%s@.%s@." s
         (String.make (String.length s) sub))
    fmt

let print fmt = Format.fprintf Format.err_formatter fmt

let inspect_reduced_decls bindings decls =
  let prev_const = ref "" in
  let print_reduced_field const fname mutabl reduced =
    if const <> !prev_const then print_header ~sub:'-' "Branch %s" const;
    print " %s%S@. reduced: @[%a@]@.@."
      (if mutabl then "mutable " else "") fname PP.inspect_reduced_type_expr reduced;
    prev_const := const in
  let print_field bindings const fname mutabl ty =
    let reduced = Gencode.beta_reduce_texpr bindings ty in
      print_reduced_field const fname mutabl reduced in

  List.iter
    (function
         Ptypes.Message_decl (name, mexpr, _) ->
           prev_const := "";
           print_header "Message %s" name;
           Gencode.iter_message bindings (print_field bindings) print_reduced_field mexpr;
       | Ptypes.Type_decl _ -> ())
    decls

let inspect_concrete bindings decls =
  let print_fields =
    List.iter
      (fun (name, mut, llty) ->
         print " %s%s : " (if mut then "mutable " else "") name;
         print "@[%a@]@.@." Sexplib.Sexp.pp_hum (Gencode.sexp_of_low_level llty)) in
  let print_msg name mexpr =
    match Gencode.low_level_msg_def bindings mexpr with
        Gencode.Message_single (_, fields) -> print_fields fields
      | Gencode.Message_sum cases ->
          List.iter
            (fun (cons, fields) -> print_header ~sub:'-' "Branch %s" cons;
                                   print_fields fields)
            cases
  in List.iter
       (function
            Ptypes.Message_decl (name, mexpr, _) -> print_header "Message %s" name;
                                                    print_msg name mexpr
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
               [] -> G.generate_code ~width:!width ?generators:!generators decls |> output_string och
             | errors ->
                 print "Found %d errors:@." (List.length errors);
                 Ptypes.pp_errors Format.err_formatter errors;
                 print "@.@]";
                 exit 1
         end;
         let bs = Gencode.collect_bindings decls in
         if !dump_reduced then inspect_reduced_decls bs decls;
         if !dump_concrete then inspect_concrete bs decls)
    !file
