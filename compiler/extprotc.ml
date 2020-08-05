
open Printf
open ExtList
open ExtString

module G = Gencode.Make(Gen_OCaml)
module PP = Gencode.Prettyprint

let (|>) x f = f x
let (@@) f x = f x

let file       = ref None
let output     = ref None
let generators = ref None
let width      = ref 100
let nolocs     = ref false
let fieldmod   = ref ""
let mli        = ref false
let export_tys = ref false
let assume_subsets = ref ""

let arg_spec =
  Arg.align
    [
      "-o", Arg.String (fun f -> output := Some f), "FILE Set output file.";

      "-g", Arg.String (fun gs -> generators := Some (String.nsplit gs ",")),
        "LIST Generators to use (comma-separated).";

      "-mli", Arg.Set mli, " Generate .mli signature.";

      "-export-type-io", Arg.Set export_tys,
        " Export (de)serialization for monomorphic record types (needed for cross-file use).";

      "-assume-subsets", Arg.Set_string assume_subsets,
        "S Enable cross-file subsets of comma-separated module list. Disables -mli. Can be 'all'. example:  -assume-subsets Msg1,Msg2";

      "-nolocs", Arg.Set nolocs,
        " Do not indicate precise locations by default in deserialization exceptions.";

      "-fieldmod", Arg.Set_string fieldmod,
        "MODULE Use MODULE for lazy fields (must be of module type Extprot.Field.S)";

      "-w", Arg.Set_int width,
        sprintf "N Set width to N characters in generated code (default: %d)." !width;
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

let header source =
  let srcline =
    let src = sprintf "%S" source in
    let s   = sprintf "(* This file was generated automatically from %s " src in
    let s   =
      if String.length s < 78 - 2 then s ^ String.make (76 - String.length s) ' '
      else s
    in
      s ^ "*)"
  in
    sprintf
      "(****************************************************************************)\n\
       %s\n\
       (* Do not edit it manually!                                                 *)\n\
       (****************************************************************************)\n\
       \n\n"
      srcline

let () =
  Arg.parse arg_spec (fun fname -> file := Some fname) usage_msg;
  match !file with
  | None -> ()
  | Some file ->
       let output = match !output with
           None -> Filename.chop_extension file ^ ".ml"
         | Some f -> f in

       if output = file then begin
         print "extprotc: refusing to overwrite %S@." file;
         exit 2
       end;

       let och      = open_out output in
       let entries  = Proto_parser.print_synerr Proto_parser.parse_file file in
       let decls    = List.filter_map (function (Ptypes.Decl decl, _) -> Some decl | _ -> None) entries in
       let bindings = Gencode.collect_bindings decls in
       let local    = List.filter_map (function (entry,Ptypes.Local) -> Some entry | (_,Ptypes.Extern) -> None) entries in

       let global_opts = if !nolocs then ["locs", "false"] else [] in
       let global_opts = match !fieldmod with
         | "" -> global_opts
         | s -> ("field-module", String.capitalize s) :: global_opts in
       let global_opts = if !export_tys then ("export_tys", "") :: global_opts else global_opts in
       let global_opts = ("assume_subsets", !assume_subsets) :: global_opts in
       let () =
         if !assume_subsets <> "" then mli := false
       in
         begin
           match Ptypes.check_declarations decls with
             | [] ->
                 let implem, signature =
                   G.generate_code
                     ~global_opts
                     ~width:!width ?generators:!generators bindings local
                 in
                   output_string och @@ header file;
                   output_string och implem;
                   if !mli then begin
                     let och = open_out (Filename.chop_extension file ^ ".mli") in
                       output_string och @@ header file;
                       output_string och signature
                   end
             | errors ->
                 print "Found %d errors:@." (List.length errors);
                 Ptypes.pp_errors Format.err_formatter errors;
                 print "@.@]";
                 exit 1
         end
