
open Printf
open Ptypes
open Gencode
open Camlp4.PreCast
open ExtList
open ExtString

type container = {
  c_name : string;
  (* either open (in message definitions) or include (in type defs), needed for
     application of record types) *)
  c_import_modules : Ast.str_item option;
  c_types : Ast.str_item option;
  c_reader : Ast.str_item option;
  c_io_reader : Ast.str_item option;
  c_pretty_printer : Ast.str_item option;
  c_writer : Ast.str_item option;
  c_default_func : Ast.str_item option;

  c_sig_import_modules : string option;
  c_sig_types : string option;
  c_sig_reader : string option;
  c_sig_pretty_printer : string option;
  c_sig_writer : string option;
  c_sig_default_func : string option;

}

and signature = string

type toplevel = Ast.str_item

type entry = Toplevel of (toplevel * string) | Container of container

let assumed_subsets opts =
  try
    List.filter ((<>) "") @@
    Str.split (Str.regexp "[,]") @@
    List.assoc "assume_subsets" opts
  with Not_found -> []

module PrOCaml =Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax)

let string_of_ast ?width f ast =
  let b = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer b in
  let o = new PrOCaml.printer () in
    Option.may (Format.pp_set_margin fmt) width;
    Format.fprintf fmt "@[<v0>%a@]@." (f o) ast;
    Buffer.contents b

let empty_container name ?default_func ?signature ty_str_item =
  {
    c_name = name;
    c_import_modules = None;
    c_types = Some ty_str_item;
    c_reader = None;
    c_io_reader = None;
    c_pretty_printer = None;
    c_writer = None;
    c_default_func = default_func;

    c_sig_import_modules = None;
    c_sig_types          = begin
      match signature with
        | Some _ as x -> x
        | None -> Some (string_of_ast (fun o -> o#implem) ty_str_item);
    end;
    c_sig_reader         = None;
    c_sig_pretty_printer = None;
    c_sig_writer         = None;
    c_sig_default_func   = begin
      match default_func with
        | None -> None
        | Some _ -> Some (Printf.sprintf "val %s_default : (unit -> %s) ref" name name)
    end
  }

let (|>) x f = f x

let smap_find_default k v m = Option.default v @@ smap_find k m

let smap_update f k m  =
  SMap.add k (f @@ smap_find k m) m

let smap_update_default f k v m =
  smap_update (fun x -> f @@ Option.default v x) k m

let foldl1 msg f g = function
    [] -> invalid_arg ("foldl1: empty list -- " ^ msg)
  | hd::tl -> List.fold_left g (f hd) tl

let foldr1 msg f g = function
    [] -> invalid_arg ("foldr1: empty list -- " ^ msg)
  | l -> match List.rev l with
        [] -> assert false
      | hd::tl -> List.fold_left (fun s x -> g x s) (f hd) tl

let paLids_of_strings _loc = List.map (fun s -> <:patt< $lid:s$ >>)

let exLids_of_strings _loc = List.map (fun s -> <:expr< $lid:s$ >>)

let paCom_of_lidlist _loc l = Ast.paCom_of_list @@ paLids_of_strings _loc l

let new_lid =
  let n = ref 0 in fun base ->
    incr n;
    base ^ "_" ^ string_of_int !n

let exTup_of_lidlist _loc l = match exLids_of_strings _loc l with
    [] -> invalid_arg "exTup_of_lidlist"
  | [e] -> e
  | hd :: tl -> <:expr< ( $hd$, $Ast.exCom_of_list tl$ ) >>

let patt_of_ll_type t =
  let _loc = Loc.mk "<genererated code @ patt_of_ll_type>" in
    <:patt< Extprot.Codec.$uid:Codec.string_of_low_level_type t$ >>

let ident_with_path _loc path ident =
  List.fold_right
    (fun p id -> <:ident< $uid:p$.$id$ >>)
    path
    <:ident< $lid:ident$ >>

let maybe_all f = function
    [] -> None
  | hd :: tl ->
      let init = Option.map (fun x -> [x]) (f hd) in
      let l = List.fold_left (fun s x -> match s with
                                  None -> None
                                | Some l -> match f x with
                                      None -> None
                                    | Some y -> Some (y :: l))
                init tl
      in Option.map List.rev l

let lookup_option name ?(global = false) (opts : type_options) =
  let pick n = (global && n = name || n = "ocaml." ^ name) in
    List.fold_left (fun _ x -> Some x) None @@
    List.filter_map (function (n, v) when pick n -> Some v | _ -> None) @@
    opts

let bad_option ?msg name v = match msg with
    Some m ->
      Printf.ksprintf failwith "Bad OCaml option value for %S: %S --- %s" name v m
  | None ->
      Printf.ksprintf failwith "Bad OCaml option value for %S: %S" name v

module Caml = Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Syntax))

let parse_string ?(verbose=true) kind entry s =
  try
    Gram.parse_string entry (Loc.mk "<string>") s
  with Loc.Exc_located (_, b) as e ->
    if verbose then
      Printf.eprintf "Parse error in OCaml %s: %s\nin\n%s\n"
        kind (Printexc.to_string b) s;
    raise e

let ctyp_of_path ?verbose s = parse_string ?verbose "type" Syntax.ctyp s
let expr_of_string ?verbose s = parse_string ?verbose "expression" Syntax.expr s

type type_info = { ty : string; ctyp : Ast.ctyp; fromf : Ast.expr; tof : Ast.expr; default : Ast.expr option; }

let hd_opt = function [] -> None | x :: _ -> Some x

let get_type_info opts = match lookup_option "type" opts with
  | None -> None
  | Some v ->
      let parses_ok ty fromf tof default =
        try
          ignore (ctyp_of_path ~verbose:false ty);
          ignore (expr_of_string ~verbose:false fromf);
          ignore (expr_of_string ~verbose:false tof);
          Option.may (fun s -> ignore (expr_of_string ~verbose:false s)) (hd_opt default);
          true
        with _ -> false
      in

      let split_with_semicolons () =
        match List.map String.strip @@ String.nsplit v ";" with
          | [ty; fromf; tof] -> ty, fromf, tof, None
          | [ty; fromf; tof; default] -> ty, fromf, tof, Some default
          | _ -> bad_option "type" v in

      let (ty,fromf,tof,default) =
        match List.map String.strip @@ String.nsplit v "," with
          | ty :: fromf :: tof :: ([] | [_] as default) ->
              if parses_ok ty fromf tof default then (ty, fromf, tof, hd_opt default)
              else split_with_semicolons ()
          | _ -> split_with_semicolons ()
      in
        try
          Some {
            ty = ty;
            ctyp = ctyp_of_path ty;
            fromf = expr_of_string fromf;
            tof = expr_of_string tof;
            default = Option.map expr_of_string default;
          }
        with exn -> bad_option ~msg:(Printexc.to_string exn) "type" v

let get_type_info opts =
  match
    lookup_option "_type__type" opts,
    lookup_option "_type__to_t" opts,
    lookup_option "_type__from_t" opts,
    lookup_option "_type__default" opts
  with
    | None, None, None, _ -> get_type_info opts
    | Some ty, Some to_t, Some from_t, default ->
        Some {
          ty;
          ctyp  = ctyp_of_path ty;
          fromf = expr_of_string to_t;
          tof   = expr_of_string from_t;
          default = Option.map expr_of_string default;
        }
    | _ ->
        failwith
          "All of ocaml._type__type, ocaml._type__to_t and \
           ocaml._type_from_t options should be set"

let get_type default opts =
  Option.map_default (fun { ctyp; _ } -> ctyp) default (get_type_info opts)

let type_opts = function
| Vint (_, opts)
| Bitstring32 opts
| Bitstring64 (_,opts)
| Bytes opts
| Sum (_, opts)
| Record (_, _, opts)
| Htuple (_, _, opts)
| Message (_, _, _, opts)
| Tuple (_, opts) -> opts

let lazy_val_overhead_estimate = 4
let thunk_overhead_estimate = 4 (* reader *) + 9 (* Fast_write.t thunk *)

let rec llty_word_size_estimate : Gencode_types.low_level -> int = function
  | Vint _ -> 1
  | Bitstring32 _ -> 3
  | Bitstring64 _ -> 3
  | Sum (cs, _) ->
      List.fold_left max 0 @@
      List.map
        (function
          | `Constant _ -> 1
          | `Non_constant (_, lltys) ->
              List.fold_left (+) 1 @@
              List.map
                (fun llty ->
                   (* Immediate values like int and bool take only the word
                    * used in the tuple; values stored in a block OTOH take
                    * the size of the block plus 1 word referencing it in the
                    * tuple. *)
                   let n = llty_word_size_estimate llty in
                     if n = 1 then 1 else n + 1)
                lltys)
        cs
  | Tuple (lltys, _) ->
      List.fold_left (+) 1 @@
      List.map
        (fun llty ->
           (* same logic as non-constant constructors in sum types *)
           let n = llty_word_size_estimate llty in
             if n = 1 then 1 else n + 1)
        lltys
  | Record (_, fs, _) ->
      List.fold_left (+) 1 @@
      List.map
        (fun f ->
           let n = llty_word_size_estimate f.field_type in
             match compute_field_ev_regime f with
               | `Eager ->
                   if n = 1 then 1 else n + 1
               | `Lazy ->
                   if deserialize_eagerly f.field_type then
                     lazy_val_overhead_estimate + (if n = 1 then 1 else n + 1)
                   else
                     thunk_overhead_estimate)
        fs
  | Message (_, _, Some llmdef, _) -> low_level_msg_def_size_estimate llmdef
  | Message (_, _, None, _) -> 1000
  | Bytes _ | Htuple _ -> 1000

and low_level_msg_def_size_estimate = function
  | Message_single (_, fs) -> field_size_estimate fs
  | Message_sum l ->
      List.fold_left max 1 @@
      List.map (fun (_, _, fs) -> field_size_estimate fs) l
  | Message_subset (_, fs, subset) ->
      field_size_estimate @@
      List.filter_map (function None -> None | Some (`Newtype x | `Orig x) -> Some x) @@
      List.map (must_keep_field subset) fs
  | Message_typealias (_, Some fs) -> field_size_estimate fs
  | Message_typealias (_, None)
  | Message_alias _ -> 1000

and field_size_estimate fs =
  List.fold_left (+) 1 @@
  List.map
    (fun (_, _, ev_regime, _, llty) ->
       let n = llty_word_size_estimate llty in
       let eagerly = deserialize_eagerly llty in
         match ev_regime with
           | `Eager ->
               if n = 1 then 1 else n + 1
           | `Auto when eagerly ->
               if n = 1 then 1 else n + 1
           | `Lazy when eagerly ->
               lazy_val_overhead_estimate + (if n = 1 then 1 else n + 1)
           | `Lazy | `Auto ->
               thunk_overhead_estimate)
    fs

and deserialize_eagerly llty =
  is_primitive_type llty ||
  lazy_val_overhead_estimate + llty_word_size_estimate llty < thunk_overhead_estimate

and is_primitive_type = function
  | Vint _ | Bitstring32 _ | Bitstring64 _ | Bytes _ -> true
  | Message _ | Sum _ | Tuple _ | Htuple _ | Record _ -> false

and compute_field_ev_regime f = match f.field_evr with
  | `Eager -> `Eager
  | `Lazy -> `Lazy
  | `Auto ->
      if deserialize_eagerly f.field_type then `Eager
      else `Lazy

let compute_field_ev_regime f = (compute_field_ev_regime f :> Gencode.ev_regime)

let compute_ev_regime_with_llty llty : [< Gencode.ev_regime] -> [> `Eager | `Lazy ]= function
  | `Eager -> `Eager
  | `Lazy -> `Lazy
  | `Auto ->
      if deserialize_eagerly llty then `Eager
      else `Lazy

let default_value_or f default opts =
  try
    Some (f @@ List.assoc "default" opts)
  with Not_found -> default

let bad_default_value ty s =
  failwith @@ sprintf "invalid %s default value: %S" ty s

let rec default_value (ev_regime : Gencode.ev_regime) t =
  let _loc = Loc.ghost in
  let default_value =
    match t with
    | Vint (Bool, opts) ->
        default_value_or
          (fun s -> match String.lowercase s with
             | "true" -> <:expr< True >>
             | "false" -> <:expr< False >>
             | _ -> bad_default_value "boolean" s)
          (Some <:expr< False >>) opts
    | Vint (Int, opts) ->
        default_value_or
          (fun s ->
             try
               ignore (int_of_string s);
               <:expr< $int:s$ >>
             with _ -> bad_default_value "int" s)
          None opts
    | Vint (Int8, opts) ->
        default_value_or
          (fun s ->
             try
               let n = int_of_string s in
                 if n < 0 || n > 255 then bad_default_value "byte" s;
                 <:expr< $int:s$ >>
             with _ -> bad_default_value "byte" s)
          None opts
    | Bytes opts ->
        default_value_or (fun s -> <:expr< $str:s$ >>) None opts
    | Bitstring64 (Long, opts) ->
        default_value_or (fun s -> <:expr< $int64:s$ >>) None opts
    | Bitstring64 (Float, opts) ->
        default_value_or (fun s -> <:expr< $flo:s$ >>) None opts
    | Bitstring32 _ -> None
    | Sum (l, _) -> begin (* first constant constructor = default*)
        match
          try
            Some (List.find_map (function `Constant x -> Some x | _ -> None) l)
          with Not_found -> None
        with
          | None -> None
          | Some c ->
              Some <:expr< $uid:String.capitalize c.const_type$.$lid:c.const_name$ >>
      end
    | Record (name, fields, _) -> begin
        Some
          (default_record
             ~namespace:name
             ~msgname:name
             (List.map
                (fun f ->
                   (* mutable flag not used, we pick false arbitrarily *)
                   (f.field_name, false, compute_field_ev_regime f, f.field_opts, f.field_type))
                fields))
      end
    | Htuple (List, _, _) -> Some <:expr< [] >>
    | Htuple (Array, _, _) -> Some <:expr< [| |] >>
    | Message (path, name, _, _) ->
        let full_path = path @ [String.capitalize name] in
        let id = ident_with_path _loc full_path (name ^ "_default") in
        let e1 = <:expr< ! $id:id$ >> in
          Some <:expr< $e1$ () >>
    | Tuple (tys, _) -> match maybe_all (default_value `Eager) tys with
          None -> None
        | Some [] -> failwith "default_value: empty tuple"
        | Some [_] -> failwith "default_value: tuple with only 1 element"
        | Some (hd::tl) -> Some <:expr< ($hd$, $Ast.exCom_of_list tl$) >>
  in
  match compute_ev_regime_with_llty t ev_regime, get_type_info @@ type_opts t, default_value with
  | _, Some { default=None; ty; _ }, Some _ -> failwith @@ sprintf "no default value specified for external type %s" ty
  | _, Some { default=None; _ }, None -> None
  | `Eager, Some { default=Some override; _ }, _ -> Some override
  | `Lazy, Some { default=Some override; _ }, _ -> Some <:expr< EXTPROT_FIELD____.from_val $override$ >>
  | _, None, None -> None
  | `Eager, None, Some v -> Some v
  | `Lazy, None, Some v -> Some <:expr< EXTPROT_FIELD____.from_val $v$ >>

and default_record ~msgname ?namespace fields =
  let _loc = Loc.ghost in
  let default_values =
    maybe_all
      (fun (name, _, ev_regime, _, llty) ->
         Option.map (fun v -> (name, v)) (default_value (ev_regime :> Gencode.ev_regime) llty))
      fields
  in match default_values with
      None -> <:expr< Extprot.Error.missing_field ~message:$str:msgname$ () >>
    | Some l ->
        let assigns =
          List.map
            (fun (name, v) -> match namespace with
                 None -> <:rec_binding< $lid:name$ = $v$ >>
               | Some ns ->
                   <:rec_binding< $uid:String.capitalize ns$.$lid:name$ = $v$ >>)
            l
        in <:expr< { $Ast.rbSem_of_list assigns$ } >>

let ident_of_ctyp ty =
  let _loc = Loc.ghost in
    try
      <:ctyp< $id:Ast.ident_of_ctyp ty$ >>
    with Invalid_argument _ -> ty

let indent n s =
  let indentation = String.make n ' ' in

  let rec doindent b s off =
    if off < String.length s then
      match String.index_from s off '\n' with
        | exception Not_found ->
            Buffer.add_string b indentation;
            Buffer.add_string b s
        | n ->
            Buffer.add_string b indentation;
            Buffer.add_substring b s off (n - off + 1);
            doindent b s (n + 1)
  in

  let b = Buffer.create 13 in
    doindent b s 0;
    Buffer.contents b

let generate_include file =
  let _loc = Loc.mk "gen_OCaml" in
  let modul = String.capitalize @@ Filename.chop_extension file in
    (<:str_item< open $uid:modul$ >>, modul)

let generate_container bindings =
  let _loc = Loc.mk "gen_OCaml" in

  let typedecl name ~opts ?(params = []) ctyp =
    let name = match List.assoc "_ppx.mangled_name" opts with
      | exception Not_found -> name
      | s -> s
    in
      Ast.TyDcl (_loc, name, params, ctyp, []) in

  let typedef name ~opts ?(params = []) ctyp =
    let ctyp = get_type ctyp opts in
    <:str_item< type $typedecl name ~opts ~params ctyp $ >> in

  let type_equals ~params ty tyname =
    let applied =
      ident_of_ctyp @@
      List.fold_left
        (fun ty tyvar -> <:ctyp< $ty$ $tyvar$ >>)
        (ctyp_of_path tyname) params
    in ident_of_ctyp <:ctyp< $applied$ == $ident_of_ctyp ty$ >> in

  let maybe_type_equals opts ~params ty =
    match lookup_option "type_equals" opts with
        None -> ty
      | Some tyname  ->
          try
            type_equals ~params ty (String.strip tyname)
          with exn -> bad_option ~msg:(Printexc.to_string exn) "type_equals" tyname in

  let keep_ppx_mangled_name l =
    List.filter (fun (k, v) -> k = "_ppx.mangled_name") l in

  let message_typedefs ~opts name ctyp =
    let internal = typedef ~opts:(keep_ppx_mangled_name opts) ("_" ^ name) @@ maybe_type_equals opts ~params:[] ctyp in
    let ext      = typedef ~opts name @@ maybe_type_equals opts ~params:[] <:ctyp< $ctyp$ >> in
      <:str_item< $internal$; $ext$ >> in

  let rec message_types ?(opts = []) msgname = function
    | `Message_record l as mexpr ->

        let llfields =
          match Gencode.low_level_msg_def bindings msgname mexpr with
            | Message_single (_, fs) ->
                List.map (fun (_, _, _, _, llty) -> llty) fs
            | _ -> failwith "low level msg def for `Message_record is not Message_single"
        in

        let ctyp ((name, mutabl, ev_regime, fopts, texpr), llty) =
          let ty = ctyp_of_texpr bindings texpr in
          let ty = match compute_ev_regime_with_llty llty ev_regime with
            | `Eager -> ty
            | `Lazy -> <:ctyp< EXTPROT_FIELD____.t $ty$ >> in

          let name = match List.assoc "_ppx.mangled_name" fopts with
            | exception Not_found -> name
            | s -> s
          in
            match mutabl with
                true -> <:ctyp< $lid:name$ : mutable $ty$ >>
              | false -> <:ctyp< $lid:name$ : $ty$ >> in
        let fields =
          foldl1 "message_types `Message_record" ctyp
            (fun ct field -> <:ctyp< $ct$; $ctyp field$ >>)
            (List.combine l llfields)
        (* no quotations for type, wtf? *)
        (* in <:str_item< type $msgname$ = { $fields$ } >> *)
        in
          (message_typedefs ~opts msgname <:ctyp< { $fields$ } >>, None)

   | `Message_app (name, args, opts) ->
       let tyname = String.capitalize name ^ "." ^ String.uncapitalize name in
       let applied =
         ident_of_ctyp @@
         List.fold_left
           (fun ty tyvar -> <:ctyp< $ty$ $tyvar$ >>)
           (ctyp_of_path tyname) (List.map (ctyp_of_texpr bindings) args)
       in
         (message_typedefs ~opts msgname <:ctyp< $applied$ >>, None)

   | `Message_alias (path, name) ->
       let full_path = path @ [String.capitalize name; name ] in
       let uid       = String.concat "." full_path in
        (message_typedefs ~opts msgname (ctyp_of_path uid), None)

   | `Message_sum l ->
       let tydef_of_msg_branch (const, mexpr) =
         <:str_item<
           module $String.capitalize const$ = struct
             $fst @@ message_types (String.lowercase const) (mexpr :> message_expr)$
           end; >> in

       let sig_of_msg_branch (const, mexpr) =
         let sig_body =
           indent 2 @@
           string_of_ast (fun o -> o#implem)
             <:str_item<
               $fst @@ message_types (String.lowercase const) (mexpr :> message_expr)$
             >>
         in
           sprintf "module %s : sig\n%s\nend\n\n"
             (String.capitalize const)
             sig_body
       in

       let record_types =
         foldl1 "message_types `Sum" tydef_of_msg_branch
           (fun s b -> <:str_item< $s$; $tydef_of_msg_branch b$ >>) l in

       let variant (const, _) =
         <:ctyp< $uid:const$ of ($uid:const$.$lid:String.lowercase const$) >> in
       let consts = foldl1 "message_types `Message_sum" variant
                      (fun vars c -> <:ctyp< $vars$ | $variant c$ >>) l in
       let signature =
         String.concat "" @@
         List.concat
           [
             List.map sig_of_msg_branch l;
             [ string_of_ast (fun o -> o#implem)
                 (message_typedefs ~opts msgname <:ctyp< [$consts$] >>);
             ]
           ]
       in
         (<:str_item<
           $record_types$;
           $message_typedefs ~opts msgname <:ctyp< [$consts$] >>$
          >>,
          Some signature)

   | `Message_subset (name, selection, sign) as mexpr ->

       let bindings, l =
         match smap_find name bindings with
           | Some (Message_decl (_, `Message_record l, _, _)) ->
               (bindings, l)

           | Some (Message_decl (_, `Message_app (name, args, _), _, _)) -> begin
               match beta_reduced_msg_app_fields bindings name args with
                 | None ->
                     exit_with_error
                       "wrong message subset %s: %s is not a simple message" msgname name

                 | Some (bindings, l) -> (bindings, l)
             end

           | None | Some _ ->
               exit_with_error
                 "wrong message subset: %s is not a simple message" name in

       let subset = subset_of_selection selection sign in

       (* check that subset is not empty *)
       let () =
         match List.filter_map (must_keep_field subset) l with
           | _ :: _ -> ()
           | [] -> exit_with_error "Message subset %s is empty." msgname
       in

       (* check that all fields referenced in subset exist *)
       let () =
         let known = List.map (fun (name, _, _, _, _) -> name) l in
           List.iter
             (fun (field, _) ->
                if not @@ List.mem field known then
                  exit_with_error
                    "Unknown field %s referenced in message subset %s."
                    field msgname)
             selection in

       let llfields =
         match Gencode.low_level_msg_def bindings msgname mexpr with
           | Message_subset (_, fs, _) ->
               List.fold_left
                 (fun m (fname, _, _, _, fllty) -> SMap.add fname fllty m)
                 SMap.empty fs
           | _ -> failwith "low level msg def for `Message_record is not Message_subset"
       in

       let ctyp (name, mutabl, ev_regime, _, texpr) =
         let ty = ctyp_of_texpr bindings texpr in
         let ty = match compute_ev_regime_with_llty (SMap.find name llfields) ev_regime with
           | `Eager -> ty
           | `Lazy -> <:ctyp< EXTPROT_FIELD____.t $ty$ >>
         in
           match mutabl with
             true -> <:ctyp< $lid:name$ : mutable $ty$ >>
           | false -> <:ctyp< $lid:name$ : $ty$ >> in

       let fields =
         foldl1 "message_types `Message_subset" ctyp
           (fun ct field -> <:ctyp< $ct$; $ctyp field$ >>) @@
         List.map subset_field @@
         List.filter_map (must_keep_field subset) l
       in
         (message_typedefs ~opts msgname <:ctyp< { $fields$ } >>, None)

  and modules_to_include_of_texpr = function
     | `App (name, _, _) ->
         let n = String.capitalize name in
           Some (<:str_item< include $uid:n$ >>, n)
     | #type_expr -> None

  and ctyp_of_texpr bindings expr =
    reduce_to_poly_texpr_core bindings expr |> ctyp_of_poly_texpr_core bindings

  and ctyp_of_poly_texpr_core bindings = function
      `Bool opts -> get_type <:ctyp< bool >> opts
    | `Byte opts -> get_type <:ctyp< int >> opts
    | `Int opts -> get_type <:ctyp< int >> opts
    | `Long_int opts -> get_type <:ctyp< Int64.t >> opts
    | `Float opts -> get_type <:ctyp< float >> opts
    | `String opts -> get_type <:ctyp< string >> opts
    | `List (ty, opts) -> get_type <:ctyp< list ($ctyp_of_poly_texpr_core bindings ty$) >> opts
    | `Array (ty, opts) -> get_type <:ctyp< array ($ctyp_of_poly_texpr_core bindings ty$) >> opts
    | `Tuple (l, opts) -> begin match l with
          [] -> failwith "ctyp_of_poly_texpr_core: empty tuple"
        | [_] -> failwith "ctyp_of_poly_texpr_core: 1-element tuple"
        | [a; b] ->
            get_type
              <:ctyp< ( $ ctyp_of_poly_texpr_core bindings a$ * $ctyp_of_poly_texpr_core bindings b$ ) >>
              opts
        | hd::tl ->
            let tl' =
              foldr1 "ctyp_of_poly_texpr_core `Tuple" (ctyp_of_poly_texpr_core bindings)
                (fun ptexpr tup -> <:ctyp< $ ctyp_of_poly_texpr_core bindings ptexpr $ * $tup$ >>)
                tl
            in get_type
                 <:ctyp< ( $ ctyp_of_poly_texpr_core bindings hd $ * $tl'$ ) >>
                 opts
      end
    | `Ext_type (path, name, args, opts) ->
        let full_path = path @ [String.capitalize name] in
        let id = ident_with_path _loc full_path name in
        let t = List.fold_left (* apply *)
                  (fun ty ptexpr -> <:ctyp< $ty$ $ctyp_of_poly_texpr_core bindings ptexpr$ >>)
                  <:ctyp< $id:id$ >>
                  args
        in get_type
             (try <:ctyp< $id:Ast.ident_of_ctyp t$ >> with Invalid_argument _ -> t)
             opts
    | `Type (name, params, args, opts) ->
        let args = List.map (ctyp_of_poly_texpr_core bindings) args in
        let t = List.fold_left (* apply *)
                  (fun ty ty_arg -> <:ctyp< $ty$ $ty_arg$ >>)
                  <:ctyp< $uid:String.capitalize name$.$lid:name$ >>
                  args
        in
        begin match get_type_info opts with
        | None ->
          (try <:ctyp< $id:Ast.ident_of_ctyp t$ >> with Invalid_argument _ -> t)
        | Some { ctyp = t; _ } ->
          (* Substitute free variables in "ocaml.type" annotation
             with specific [args] from type application *)
          assert (List.length args = List.length params); (* checked in Ptypes *)
          let params_map = List.combine (List.map type_param_name params) args in
          let substitute = function
          | <:ctyp@_loc< '$param$ >> ->
            begin try
              <:ctyp< $List.assoc param params_map$ >>
            with
            | Not_found -> failwith @@ sprintf "Unbound type parameter '%s in %s" param name
            end
          | t -> t
          in
          (Ast.map_ctyp substitute)#ctyp t
        end
    | `Type_arg n -> <:ctyp< '$n$ >>

  in function
    | Message_decl (msgname, mexpr, _, opts) -> begin
        let wrap x =
          match get_type_info opts with
              None -> x
            | Some { fromf; _ } ->
                <:expr< $fromf$ $x$ >> in

        let default_func =
          match Gencode.low_level_msg_def bindings msgname mexpr with

          | Message_single (namespace, fields) ->
              <:str_item<
                value $lid:msgname ^ "_default"$ : ref (unit -> $lid:msgname$) =
                  ref (fun () -> $ wrap (default_record ~msgname ?namespace fields) $)
              >>

          | Message_alias (path, name) ->
              let full_path = path @ [String.capitalize name] in
              let v = <:expr< $id:ident_with_path _loc full_path (name ^ "_default") $ >> in
              let v = <:expr< $v$.val () >> in
                <:str_item<
                  value $lid:msgname ^ "_default"$ = ref (fun () -> $wrap v$)
                >>

          | Message_typealias (name, _) ->
              let full_path = [String.capitalize name] in
              let v = <:expr< $id:ident_with_path _loc full_path (name ^ "_default") $ >> in
              let v = <:expr< $v$.val () >> in
                <:str_item<
                  value $lid:msgname ^ "_default"$ = ref (fun () -> $wrap v$)
                >>

          | Message_sum ((namespace, constr, fields) :: _) ->
              let namespace = Option.default constr namespace in
              let v =
                <:expr< $uid:String.capitalize constr$
                           $ default_record ~msgname ~namespace fields $ >>
              in
                <:str_item<
                  value $lid:msgname ^ "_default"$ : ref (unit -> $lid:msgname$) =
                    ref (fun () -> $wrap v$)
                >>
          | Message_sum [] -> failwith "bug in generate_container: empty Message_sum list"

          | Message_subset (_, l, subset) ->
            let l = List.map subset_field @@ List.filter_map (must_keep_field subset) l in
              <:str_item<
                value $lid:msgname ^ "_default"$ : ref (unit -> $lid:msgname$) =
                  ref (fun () -> $ wrap (default_record ~msgname l) $)
              >> in

        let str_tys, signature = message_types ~opts msgname mexpr in
        let container = empty_container msgname ~default_func ?signature str_tys in
          Some container
      end
    | Type_decl (name, params, texpr, opts) ->
        let ty = match poly_beta_reduce_texpr bindings texpr with
          | `Record (r, _) -> begin
              let ctyp (name, mutabl, ev_regime, fopts, texpr) =
                let ty = ctyp_of_poly_texpr_core bindings texpr in
                let ty = match ev_regime with
                  | `Eager | `Auto -> ty
                  | `Lazy -> <:ctyp< EXTPROT_FIELD____.t $ty$ >> in

                let name = match List.assoc "_ppx.mangled_name" fopts with
                  | exception Not_found -> name
                  | s -> s
                in
                  match mutabl with
                    true -> <:ctyp< $lid:name$ : mutable $ty$ >>
                  | false -> <:ctyp< $lid:name$ : $ty$ >> in
              let fields =
                foldl1 "ctyp_of_poly_texpr_core `Type_decl (_, _, `Record _, _)" ctyp
                  (fun ct field -> <:ctyp< $ct$; $ctyp field$ >>) r.record_fields
              in <:ctyp< { $fields$ } >>
            end
          | `Sum (s, _) -> begin
              let ty_of_const_texprs (const, ptexprs) =
                (* eprintf "type %S, const %S, %d ptexprs\n" name const (List.length ptexprs); *)
                let tys = List.map (ctyp_of_poly_texpr_core bindings) ptexprs in
                  <:ctyp< $uid:const$ of $Ast.tyAnd_of_list tys$>>

              in let sum_ty =
                foldl1 "generate_container Type_decl `Sum"
                  (function
                     | `Constant tyn -> <:ctyp< $uid:tyn$ >>
                     | `Non_constant l -> ty_of_const_texprs l)
                  (fun ctyp c -> match c with
                     | `Constant tyn -> <:ctyp< $ctyp$ | $uid:tyn$ >>
                     | `Non_constant c -> <:ctyp< $ctyp$ | $ty_of_const_texprs c$>>)
                  s.constructors
                (*
                match s.constant with
                  [] -> foldl1 "generate_container Type_decl `Sum"
                          ty_of_const_texprs
                          (fun ctyp c -> <:ctyp< $ctyp$ | $ty_of_const_texprs c$ >>)
                          s.non_constant
                | _ ->
                    let const =
                      foldl1 "generate_container `Sum"
                        (fun tyn -> <:ctyp< $uid:tyn$ >>)
                        (fun ctyp tyn -> <:ctyp< $ctyp$ | $uid:tyn$ >>)
                        s.constant

                    in List.fold_left
                         (fun ctyp c -> <:ctyp< $ctyp$ | $ty_of_const_texprs c$ >>)
                         const s.non_constant
                 *)
              in <:ctyp< [ $sum_ty$ ] >>
            end
          | #poly_type_expr_core as ptexpr ->
              get_type (ctyp_of_poly_texpr_core bindings ptexpr) opts in
        let params =
          List.map (fun n -> <:ctyp< '$lid:type_param_name n$ >>) params in
        let type_rhs   = maybe_type_equals opts ~params ty in
        let container  = empty_container name (typedef name ~params ~opts type_rhs) in
        let mods, sigs = match modules_to_include_of_texpr texpr with
          | Some (a, b) -> Some a, Some b
          | None -> None, None
        in
          Some { container with
                   c_import_modules = mods;
                   c_sig_import_modules = Option.map (sprintf "open %s") sigs;
               }

let loc = Camlp4.PreCast.Loc.mk

let maybe_str_item =
  let _loc = loc "<generated code>" in
    Option.default <:str_item< >>

let default_function = function
    None -> None
  | Some _ -> assert false

let generate_code ?(global_opts=[]) ?width containers =
  let _loc = loc "<generated code>" in
  let container_of_str_item c =
    <:str_item<
       module $String.capitalize c.c_name$ = struct
         $maybe_str_item c.c_import_modules$;
         $maybe_str_item c.c_types$;
         $maybe_str_item c.c_default_func$;
         $maybe_str_item c.c_pretty_printer$;
         $maybe_str_item c.c_reader$;
         $maybe_str_item c.c_io_reader$;
         $maybe_str_item c.c_writer$
       end >> in

  let field_mod_declaration =
    match List.assoc "field-module" global_opts with
      | exception Not_found ->
          <:str_item< module EXTPROT_FIELD____ = Extprot.Field >>
      | modpath ->
          let rec split_modpath ?(acc=[]) = function
            | "" -> List.rev acc
            | s ->
                match String.index s '.' with
                  | exception Not_found -> List.rev @@ s :: acc
                  | n ->
                      split_modpath
                        ~acc:(String.sub s 0 n :: acc)
                        (String.sub s (n + 1) (String.length s - n - 1))
          in
            let mod_uid =
              match List.rev @@ split_modpath modpath with
                | [] -> failwith "empty fieldmod path"
                | [m] -> <:module_expr< $uid:m$ >>
                | m :: ms ->
                    let id =
                      List.fold_left
                        (fun id p -> <:ident< $uid:p$.$id$ >>)
                        <:ident< $uid:m$ >>
                        ms
                    in
                      <:module_expr< $id:id$ >>
          in
            <:str_item< module EXTPROT_FIELD____ = $mod_uid$ >> in

  let implem_code =
    string_of_ast ?width (fun o -> o#implem)
       (List.fold_left
          begin fun s e ->
            match e with
            | Toplevel (t, _) -> <:str_item< $s$; $t$; >>
            | Container c -> <:str_item< $s$; $container_of_str_item c$ >>
          end
          field_mod_declaration
          containers) in

  let field_mod =
    match List.assoc "field-module" global_opts with
      | exception Not_found -> "Extprot.Field"
      | s -> s in

  let extprot_field_re = Str.regexp "EXTPROT_FIELD____" in

  let sig_code =
    Str.global_replace extprot_field_re field_mod @@
    String.concat "\n\n" @@
    List.map
      (function
        | Toplevel (_, modul) -> sprintf "open %s" modul
        | Container c ->
            let sig_body =
              String.concat "\n" @@
              List.map (indent 2) @@
              List.filter ((<>) "")
                [
                  Option.default "" c.c_sig_import_modules;
                  Option.default "" c.c_sig_types;
                  Option.default "" c.c_sig_default_func;
                  Option.default "" c.c_sig_pretty_printer;
                  Option.default "" c.c_sig_reader;
                  Option.default "" c.c_sig_writer
                ]
            in
              sprintf
                "module %s : sig\n\
                 %s\n\
                 end"
                (String.capitalize c.c_name)
                sig_body)
      containers;
  in
    (implem_code, sig_code)

let wrap_value opts expr =
  let _loc = Loc.ghost in
  match get_type_info opts with
  | Some { tof; _ } -> <:expr< $tof$ $expr$ >>
  | None -> expr

module Pretty_print =
struct
  let _loc = Loc.mk "Gen_OCaml.Pretty_print"

  let expr_of_list l =
    List.fold_right (fun x l -> <:expr< [ $x$ :: $l$ ] >>) l <:expr< [] >>

  let pp_func name = <:expr< Extprot.Pretty_print.$lid:name$ >>

  let pp_name path name = <:expr< $id:ident_with_path _loc path ("pp_" ^ name)$ >>

  let rec pp_message ?namespace bindings msgname = function
    | `Message_record l as mexpr ->
        let ev_regimes =
          match Gencode.low_level_msg_def bindings msgname mexpr with
            | Message_single (_, fs) ->
                List.fold_left
                  (fun m (name, _, evr, _opts, llty) ->
                     SMap.add name (compute_ev_regime_with_llty llty evr) m)
                  SMap.empty fs
            | _ -> failwith "low level msg def for `Message_record is not Message_single"
        in
          pp_message_record ?namespace bindings ~ev_regimes msgname l

    | (`Message_subset (name, selection, sign) : message_expr) as mexpr ->
        let bindings, l =
          match smap_find name bindings with
            | Some (Message_decl (_, `Message_record l, _, _)) ->
                  (bindings, l)

            | Some (Message_decl (_, `Message_app (name, args, _), _, _)) -> begin
                match beta_reduced_msg_app_fields bindings name args with
                  | None ->
                      exit_with_error
                        "wrong message subset %s: %s is not a simple message" msgname name

                  | Some (bindings, l) -> (bindings, l)
              end

            | None | Some _ ->
                exit_with_error
                  "wrong message subset %s: %s is not a simple message" msgname name in

        let ev_regimes =
          match Gencode.low_level_msg_def bindings msgname mexpr with
            | Message_subset (_, fs, _) ->

                let subset = subset_of_selection selection sign in

                List.fold_left
                  (fun m (name, _, evr, _opts, llty) ->
                     (* take into account ev regime overrides in subset *)
                     let evr = match subset with
                       | Exclude_fields _ -> evr
                       | Include_fields l ->
                           try
                             match List.assoc name l with
                               | (_, Some evr) -> evr
                               | (_, None) -> evr
                           with Not_found -> evr
                     in
                       SMap.add name (compute_ev_regime_with_llty llty evr) m)
                  SMap.empty fs
            | _ -> failwith "low level msg def for `Message_record is not Message_subset"
        in
          pp_message_record bindings ~ev_regimes msgname @@
          List.map subset_field @@
          List.filter_map (must_keep_field @@ subset_of_selection selection sign) l

    | `Message_app(name, args, _) ->
        let pp_func =
          List.fold_left
            (fun e ptexpr -> <:expr< $e$ $pp_texpr bindings ptexpr$ >>)
            (pp_name [String.capitalize name] name)
            args
        in <:expr< $pp_func$ pp >>
    | `Message_alias (path, name) ->
        let full_path = path @ [String.capitalize name] in
        <:expr< $pp_name full_path name$ pp >>
    | `Message_sum l ->
        let match_case (const, mexpr) =
          <:match_case<
            $uid:const$ t ->
              $pp_message ~namespace:const bindings msgname (mexpr :> message_expr)$ t>>
        in <:expr< fun [ $Ast.mcOr_of_list (List.map match_case l)$ ] >>

  and pp_message_record ?namespace bindings ~ev_regimes msgname l =
    let pp_field i (name, _, _, _, tyexpr) =
      let selector = match namespace, SMap.find name ev_regimes with
        | None, `Eager -> <:expr< (fun t -> t.$lid:name$) >>
        | Some ns, `Eager -> <:expr< (fun t -> t.$uid:String.capitalize ns$.$lid:name$) >>
        | None, `Lazy -> <:expr< (fun t -> EXTPROT_FIELD____.force t.$lid:name$) >>
        | Some ns, `Lazy -> <:expr< (fun t -> EXTPROT_FIELD____.force t.$uid:String.capitalize ns$.$lid:name$) >>
      in
      let prefix = match namespace with
          None -> String.capitalize msgname
        | Some ns -> sprintf "%s.%s"
                       (String.capitalize msgname) (String.capitalize ns) in
      let label =
        if i = 0 then prefix ^ "." ^ name
        else name
      in <:expr<
               ( $str:label$,
                 $pp_func "pp_field"$ $selector$ $pp_texpr bindings tyexpr$ )
             >> in
    let pp_fields = List.mapi pp_field l in
      <:expr< $pp_func "pp_struct"$ $expr_of_list pp_fields$ pp >>

  and pp_texpr bindings texpr =
    reduce_to_poly_texpr_core bindings texpr |> pp_poly_texpr_core bindings

  and pp_poly_type bindings path name args =
    let path = path @ [String.capitalize name] in
    List.fold_left
      (fun e ptexpr -> <:expr< $e$ $pp_poly_texpr_core bindings ptexpr$ >>)
      (pp_name path name)
      args

  and pp_poly_texpr_core bindings = function
      `Bool _ -> pp_func "pp_bool"
    | `Byte _ -> pp_func "pp_int"
    | `Int _ -> pp_func "pp_int"
    | `Long_int _ -> pp_func "pp_int64"
    | `Float _ -> pp_func "pp_float"
    | `String _ -> pp_func "pp_string"
    | `List (ty, _) -> <:expr< $pp_func "pp_list"$ $pp_poly_texpr_core bindings ty$ >>
    | `Array (ty, _) -> <:expr< $pp_func "pp_array"$ $pp_poly_texpr_core bindings ty$ >>
    | `Tuple ([ty], _) -> pp_poly_texpr_core bindings ty
    | `Tuple (l, _) ->
        List.fold_left
          (fun e ptexpr -> <:expr< $e$ $pp_poly_texpr_core bindings ptexpr$ >>)
          (pp_func ("pp_tuple" ^ string_of_int (List.length l)))
          l
    | `Type (name, _params, args, _) -> pp_poly_type bindings [] name args
    | `Ext_type (path, name, args, _) -> pp_poly_type bindings path name args
    | `Type_arg n -> <:expr< $lid:"pp_" ^ n$ >>

  let add_msgdecl_pretty_printer ~export:_ bindings msgname mexpr opts c =
    let expr = pp_message bindings msgname mexpr in
    { c with
        c_pretty_printer =
          Some
            <:str_item<
              value $lid:"pp_" ^ msgname$ pp x = $expr$ ($wrap_value opts <:expr<x>>$);
              value pp = $lid:"pp_" ^ msgname$;
            >> ;
          c_sig_pretty_printer =
            Some (Printf.sprintf
                    "val pp_%s : Format.formatter -> %s -> unit\n\
                     val pp : Format.formatter -> %s -> unit\n"
                    msgname msgname msgname)
}

  let add_typedecl_pretty_printer bindings tyname typarams texpr opts c =
    let wrap expr =
      List.fold_right
        (fun typaram e ->
           <:expr< fun $lid:"pp_" ^ type_param_name typaram$ -> $e$ >>)
        typarams
        expr in

    let expr = match poly_beta_reduce_texpr bindings texpr with
      | `Sum (s, opts) -> begin
          let constr_ptexprs_case (const, ptexprs) =
            let params = Array.to_list @@
                         Array.init (List.length ptexprs) (sprintf "v%d")
            in
              <:match_case<
                $uid:const$ $paCom_of_lidlist _loc params$ ->
                  $pp_func "fprintf"$ pp
                  $str:String.capitalize tyname ^ "." ^ const ^ " %a"$
                  $pp_poly_texpr_core bindings (`Tuple (ptexprs, opts))$ ($exTup_of_lidlist _loc params$)
              >> in
          let constr_case constr =
            <:match_case<
                $uid:constr$ -> $pp_func "fprintf"$ pp
                                  $str:String.capitalize tyname ^ "." ^ constr$
            >> in
          let cases = List.map
                        (function
                           | `Constant c -> constr_case c
                           | `Non_constant (n, l) -> constr_ptexprs_case (n, l))
                        s.constructors
          in
          <:expr< fun pp -> fun x -> match ($wrap_value opts <:expr<x>>$) with [ $Ast.mcOr_of_list cases$ ] >>
        end
      | `Record (r, opts) -> begin
          let pp_field i (name, _, ev_regime, _, tyexpr) =
            let field_name =
              if i = 0 then String.capitalize r.record_name ^ "." ^ name
              else name in

            let selector = match ev_regime with
              | `Auto | `Eager -> <:expr< (fun t -> t.$lid:name$) >>
              | `Lazy ->  <:expr< (fun t -> EXTPROT_FIELD____.force t.$lid:name$) >>
            in
              <:expr<
                ( $str:field_name$,
                  $pp_func "pp_field"$ $selector$ $pp_poly_texpr_core bindings tyexpr$ )
              >> in
          let pp_fields = List.mapi pp_field r.record_fields in
          <:expr< fun ppf -> fun x -> $pp_func "pp_struct"$ $expr_of_list pp_fields$ ppf ($wrap_value opts <:expr<x>>$) >>
        end
      | #poly_type_expr_core as ptexpr ->
          let ppfunc_expr = pp_poly_texpr_core bindings ptexpr in
          <:expr< fun ppf -> fun x -> $ppfunc_expr$ ppf ($wrap_value opts <:expr<x>> $) >>
    in
      { c with c_pretty_printer =
          Some <:str_item< value $lid:"pp_" ^ tyname$ = $wrap expr$ >> }

  let add_typedecl_pretty_printer bindings tyname typarams texpr opts c =
    match lookup_option "pp" opts with
        None -> add_typedecl_pretty_printer bindings tyname typarams texpr opts c
      | Some s ->
      { c with c_pretty_printer =
          Some <:str_item< value $lid:"pp_" ^ tyname$ = $expr_of_string s$ >> }
end

let partition_constructors l =
  let c = List.filter_map (function `Constant x -> Some x | _ -> None) l in
  let nc = List.filter_map (function `Non_constant x -> Some x | _ -> None) l in
    (c, nc)

module type READER_OPS =
sig
  val name        : string
  val reader_func : Reader.reader_func -> Ast.expr
  val get_string_subreader : Ast.expr option
  val raw_rd_func : low_level -> (Ast.patt * Ast.expr) option
  val read_msg_func : string -> string
end

let raw_rd_func reader_func =
  let _loc = Loc.ghost in
  let wrap t readerf = match get_type_info @@ type_opts t with
        Some { fromf; _ } ->
          <:expr<
            (fun s ->
               try
                 $fromf$ ($readerf$ s)
               with [ Extprot.Error.Extprot_error _ as e -> raise e
                    | e -> Extprot.Error.conversion_error e])
          >>
      | None -> readerf in
  let patt = patt_of_ll_type in
  let module C = Codec in
  fun t ->
    let x = match t with
    | Vint (Bool, _) -> Some (patt C.Bits8, reader_func `Read_raw_bool)
    | Vint (Int8, _) -> Some (patt C.Bits8, reader_func `Read_raw_i8)
    | Vint (Int, _) -> Some (patt C.Vint, reader_func `Read_raw_rel_int)
    | Bitstring32 _ -> Some (patt C.Bits32, reader_func `Read_raw_i32)
    | Bitstring64 (Long, _) -> Some (patt C.Bits64_long, reader_func `Read_raw_i64)
    | Bitstring64 (Float, _) -> Some (patt C.Bits64_float, reader_func `Read_raw_float)
    | Bytes _ -> Some (patt C.Bytes, reader_func `Read_raw_string)
    | Sum _ | Record _ | Tuple _ | Htuple _ | Message _ -> None
    in
    match x with
    | None -> None
    | Some (pat, reader) -> Some (pat, wrap t reader)

module STR_OPS : READER_OPS =
struct
  let _loc = Loc.mk "<generated code @ add_message_reader>"
  let name = "STR"
  let reader_func t =
    <:expr< Extprot.Reader.String_reader.
              $lid:Reader.string_of_reader_func t$ >>

  let raw_rd_func = raw_rd_func reader_func

  let get_string_subreader =
    Some <:expr< Extprot.Reader.String_reader.make_sub >>

  let read_msg_func = ((^) "read_")
end

type field_info = string * bool * [`Auto | `Eager | `Lazy ] * field_opts * Gencode.low_level

module type READER =
sig
  val read_message :
    ?inline:bool ->
    field_reader_func_uses : (Gencode.msg_name * [`Auto | `Eager | `Lazy]) list SMap.t SMap.t ->
    Gencode.msg_name ->
    Ptypes.type_options ->
    Gencode.low_level Gencode.message ->
    Camlp4.PreCast.Ast.str_item * Ast.expr

  val read :
    Gencode.msg_name ->
    Gencode.constructor_name ->
    Gencode.field_name ->
    fieldno:int ->
    ev_regime:[`Eager | `Lazy] ->
    Gencode.low_level -> Ast.expr

  val read_sum_elms :
    Gencode.msg_name ->
    Gencode.constructor_name ->
    Gencode.field_name ->
    Gencode.constructor ->
    (Gencode.low_level * Ast.expr option) list -> Ast.expr

  val read_tuple_elms :
    Gencode.msg_name ->
    Gencode.constructor_name ->
    Gencode.field_name ->
    (Gencode.low_level * Ast.expr option) list -> Ast.expr

  val make_promoted_match_cases :
    string ->
    (string option * string option * field_info list) list -> Ast.match_case

  val record_case_inlined :
    string -> locs:bool -> ?namespace:string -> ?constr:string -> int ->
    field_info list -> Ast.match_case

  val wrap_msg_reader :
    Gencode.msg_name ->
    ?promoted_match_cases:Ast.match_case ->
    Ast.match_case -> Ast.expr
end

let rec may_use_hint_path = function
  | Vint _ | Bitstring32 _ | Bitstring64 _ | Bytes _ -> false
  | Message (_, _, None, _) -> true
  | Message (_, _, Some mexpr, _) -> msg_may_use_hint_path mexpr
  | Sum (cs, _) ->
      List.exists
        (function
          | `Constant _ -> false
          | `Non_constant (_, lltys) -> List.exists may_use_hint_path lltys)
        cs
  | Tuple (lltys, _) -> List.exists may_use_hint_path lltys
  | Htuple (_, llty, _) -> may_use_hint_path llty
  | Record (_, fs, _) -> List.exists (fun f -> may_use_hint_path f.field_type) fs

and msg_may_use_hint_path = function
  | Message_single (_, fields)
  | Message_typealias (_, Some fields) ->
      List.exists
        (fun (_, _, ev_regime, _, llty) ->
           match compute_ev_regime_with_llty llty ev_regime with
             | `Eager -> may_use_hint_path llty
             | `Lazy -> may_use_hint_path llty || not @@ deserialize_eagerly llty)
        fields
  | Message_sum cases ->
      List.exists
        (fun (_, _, fields) ->
           List.exists
             (fun (_, _, ev_regime, _, llty) ->
                match compute_ev_regime_with_llty llty ev_regime with
                  | `Eager -> may_use_hint_path llty
                  | `Lazy -> may_use_hint_path llty || not @@ deserialize_eagerly llty)
             fields)
        cases
  | Message_alias _ -> true
  | Message_typealias _ -> true
  | Message_subset (_, fs, subset) ->
      List.exists
        (fun ((_, _, ev_regime, _, llty) as field) ->
           Option.is_some (must_keep_field subset field) &&
           begin match compute_ev_regime_with_llty llty ev_regime with
             | `Eager -> may_use_hint_path llty
             | `Lazy -> may_use_hint_path llty || not @@ deserialize_eagerly llty
           end)
        fs

module rec STRING_READER : READER = Make_reader(STR_OPS)

and Make_reader : functor (RD : READER_OPS) -> READER =
functor(RD : READER_OPS) ->
struct
  let _loc = Loc.mk "<generated code at Make_reader>"

  let use_locs opts =
    match List.assoc "locs" opts with
      | exception Not_found -> true
      | "false" -> false
      | _ -> true

  let wrap_reader opts expr = match get_type_info opts with
      Some { fromf; _ } ->
        <:expr<
          try
            $fromf$ $expr$
          with [ Extprot.Error.Extprot_error _ as e -> raise e
               | e -> Extprot.Error.conversion_error e]
        >>
    | None -> expr


  let update_path_if_needed ~name ~fieldno llty e =
    if may_use_hint_path llty then
      <:expr<
         let path = EXTPROT_FIELD____.Hint_path.append_field path $str:name$ $int:string_of_int fieldno$ in
         let _    = path in
           $e$
      >>
    else
      e

  let rec read_constructor_elms msgname constr_name name f lltys_and_defs =
    (* TODO: handle missing elms *)
    let vars = List.rev @@ Array.to_list @@
               Array.init (List.length lltys_and_defs) (sprintf "v%d")
    in
      List.fold_right
        (fun (n, llty, default) e ->
           let varname = sprintf "v%d" n in
             match default with
                 None ->
                   <:expr<
                     let $lid:varname$ =
                       if nelms >= $int:string_of_int (n+1)$ then
                         $read msgname constr_name name ~fieldno:n ~ev_regime:`Eager llty$
                       else
                         Extprot.Error.missing_tuple_element
                           ~message:$str:msgname$
                           ~constructor:$str:constr_name$
                           ~field:$str:name$
                           $int:string_of_int n$
                     in $e$
                   >>
               | Some expr ->
                   <:expr<
                     let $lid:varname$ =
                       if nelms >= $int:string_of_int (n+1)$ then
                         $read msgname constr_name name ~fieldno:n ~ev_regime:`Eager llty$
                       else $expr$
                     in $e$
                   >>)
        (List.mapi (fun i (ty, default) -> (i, ty, default)) lltys_and_defs)
        (f (List.rev vars))

  and read_tuple_elms msgname constr_name name lltys_and_defs =
    let mk_tup vars =
      Ast.exCom_of_list @@ List.map (fun v -> <:expr< $lid:v$ >>) vars
    in read_constructor_elms msgname constr_name name mk_tup lltys_and_defs

  and read_sum_elms msgname constr_name name constr lltys =
    let c = constr in
    let mk_expr vars =
      List.fold_left
        (fun e var -> <:expr< $e$ $lid:var$ >>)
        <:expr< $uid:String.capitalize c.const_type$.$uid:c.const_name$ >>
        vars
    in read_constructor_elms msgname constr_name name mk_expr lltys

  and read msgname constr_name name ~fieldno ~ev_regime t =

    let wrap expr = wrap_reader (type_opts t) expr in

      match ev_regime, t with
        | `Eager, Vint (Bool, _) -> wrap <:expr< $RD.reader_func `Read_bool$ s >>
        | `Eager, Vint (Int, _) -> wrap <:expr< $RD.reader_func `Read_rel_int$ s >>
        | `Eager, Vint (Int8, _) -> wrap <:expr< $RD.reader_func `Read_i8$ s >>
        | `Eager, Bitstring32 _ -> wrap <:expr< $RD.reader_func `Read_i32$ s >>
        | `Eager, Bitstring64 (Long, _) -> wrap <:expr< $RD.reader_func `Read_i64$ s >>
        | `Eager, Bitstring64 (Float, _) -> wrap <:expr< $RD.reader_func `Read_float$ s >>
        | `Eager, Bytes _ -> wrap <:expr< $RD.reader_func `Read_string$ s >>

        | `Lazy, Vint (Bool, _) ->
            let rd = wrap <:expr< $RD.reader_func `Read_bool$ s >> in
              <:expr< EXTPROT_FIELD____.from_val $rd$ >>
        | `Lazy, Vint (Int, _) ->
            let rd = wrap <:expr< $RD.reader_func `Read_rel_int$ s >> in
              <:expr< EXTPROT_FIELD____.from_val $rd$ >>
        | `Lazy, Vint (Int8, _) ->
            let rd = wrap <:expr< $RD.reader_func `Read_i8$ s >> in
              <:expr< EXTPROT_FIELD____.from_val $rd$ >>
        | `Lazy, Bitstring32 _ ->
            let rd = wrap <:expr< $RD.reader_func `Read_i32$ s >> in
              <:expr< EXTPROT_FIELD____.from_val $rd$ >>
        | `Lazy, Bitstring64 (Long, _) ->
            let rd = wrap <:expr< $RD.reader_func `Read_i64$ s >> in
              <:expr< EXTPROT_FIELD____.from_val $rd$ >>
        | `Lazy, Bitstring64 (Float, _) ->
            let rd = wrap <:expr< $RD.reader_func `Read_float$ s >> in
              <:expr< EXTPROT_FIELD____.from_val $rd$ >>
        | `Lazy, Bytes _ ->
            let rd = wrap <:expr< $RD.reader_func `Read_string$ s >> in
              <:expr< EXTPROT_FIELD____.from_val $rd$ >>

        | ev_regime, (Tuple (lltys, _) as llty) -> begin

            let bad_type_case =
              <:match_case< ll_type -> Extprot.Error.bad_wire_type ~ll_type () >> in

            let f__raw_rd_func, f__reader_func, f__read_tuple_elms =
              match ev_regime with
                | `Eager -> RD.raw_rd_func, RD.reader_func, read_tuple_elms
                | `Lazy ->
                    STR_OPS.raw_rd_func, STR_OPS.reader_func,
                    STRING_READER.read_tuple_elms
            in

            let other_cases = match lltys with
                [ty] -> begin match f__raw_rd_func ty with
                    Some (mc, reader_expr) ->
                      <:match_case< $mc$ -> $reader_expr$ s | $bad_type_case$ >>
                  | None -> bad_type_case
                end
              (* handle missing elements when expanding the primitive type to
               * a Tuple; needs default values *)
              | ty :: tys -> begin match f__raw_rd_func ty with
                  | Some (mc, reader_expr) -> begin match maybe_all (default_value `Eager) tys with
                        None -> bad_type_case
                      | Some defs ->
                          <:match_case<
                              $mc$ -> ($reader_expr$ s, $Ast.exCom_of_list defs$)
                            | $bad_type_case$
                          >>
                    end
                  | None -> bad_type_case
                end
              | _ -> (* can't happen *) bad_type_case in

            let other_cases_wrapped_lazy_eager_loading = match lltys with
                [ty] -> begin match RD.raw_rd_func ty with
                    Some (mc, reader_expr) ->
                      let e = <:expr< $reader_expr$ s >> in
                        <:match_case< $mc$ -> EXTPROT_FIELD____.from_val ($wrap e$) | $bad_type_case$ >>
                  | None -> bad_type_case
                end
              (* handle missing elements when expanding the primitive type to
               * a Tuple; needs default values *)
              | ty :: tys -> begin match RD.raw_rd_func ty with
                  | Some (mc, reader_expr) -> begin match maybe_all (default_value `Eager) tys with
                        None -> bad_type_case
                      | Some defs ->
                          let e = <:expr< ($reader_expr$ s, $Ast.exCom_of_list defs$) >> in
                            <:match_case<
                                $mc$ -> EXTPROT_FIELD____.from_val $wrap e$
                              | $bad_type_case$
                            >>
                    end
                  | None -> bad_type_case
                end
              | _ -> (* can't happen *) bad_type_case in

            let tys_with_defvalues =
              List.map (fun llty -> (llty, default_value `Eager llty)) lltys
            in
              match ev_regime with
                | `Eager ->
                    update_path_if_needed ~name ~fieldno llty @@
                    wrap @@
                    <:expr<
                       let t = $f__reader_func `Read_prefix$ s in
                         match Extprot.Codec.ll_type t with [
                             Extprot.Codec.Tuple ->
                               let len = $f__reader_func `Read_vint$ s in
                               let eot = $f__reader_func `Offset$ s len in
                               let nelms = $f__reader_func `Read_vint$ s in
                               let v = $f__read_tuple_elms msgname constr_name name tys_with_defvalues$ in begin
                                 $f__reader_func `Skip_to$ s eot;
                                 v
                               end
                           | $other_cases$
                         ]
                     >>

                | `Lazy when deserialize_eagerly llty ->
                    update_path_if_needed ~name ~fieldno llty @@
                    <:expr<
                       let t = $RD.reader_func `Read_prefix$ s in
                         match Extprot.Codec.ll_type t with [
                             Extprot.Codec.Tuple ->
                               let len = $RD.reader_func `Read_vint$ s in
                               let eot = $RD.reader_func `Offset$ s len in
                               let nelms = $RD.reader_func `Read_vint$ s in
                               let v = $read_tuple_elms msgname constr_name name tys_with_defvalues$ in begin
                                 $RD.reader_func `Skip_to$ s eot;
                                 EXTPROT_FIELD____.from_val ($wrap <:expr< v >>$)
                               end
                           | $other_cases_wrapped_lazy_eager_loading$
                         ]
                     >>

                | `Lazy ->

                    let e =
                      <:expr<
                         let t = $STR_OPS.reader_func `Read_prefix$ s in
                           match Extprot.Codec.ll_type t with [
                               Extprot.Codec.Tuple ->
                                 let len = $STR_OPS.reader_func `Read_vint$ s in
                                 let eot = $STR_OPS.reader_func `Offset$ s len in
                                 let nelms = $STR_OPS.reader_func `Read_vint$ s in
                                 let v = $f__read_tuple_elms msgname constr_name name tys_with_defvalues$ in begin
                                   $STR_OPS.reader_func `Skip_to$ s eot;
                                   v
                                 end
                             | $other_cases$
                           ]
                        >>
                    in
                      <:expr<
                         let s = $RD.reader_func `Get_value_reader$ s in
                         let path = EXTPROT_FIELD____.Hint_path.append_field path $str:name$ $int:string_of_int fieldno$ in
                           EXTPROT_FIELD____.from_fun ?hint ~level ~path s begin fun s ->
                             $wrap e$
                          end
                        >>
          end

        | ev_regime, (Sum (constructors, _) as llty) -> begin

            let f__raw_rd_func, f__reader_func, f__read_sum_elms, ev_regime =
              match ev_regime with
                | `Eager -> RD.raw_rd_func, RD.reader_func, read_sum_elms, `Eager
                | `Lazy when deserialize_eagerly llty ->
                    RD.raw_rd_func, RD.reader_func, read_sum_elms, `Lazy_Immediate
                | `Lazy ->
                    STR_OPS.raw_rd_func, STR_OPS.reader_func,
                    STRING_READER.read_sum_elms, `Lazy
            in

            let constant, non_constant = partition_constructors constructors in
            let constant_match_cases =
              List.map
                (fun c ->
                   <:match_case<
                     $int:string_of_int c.const_tag$ ->
                       $uid:String.capitalize c.const_type$.$lid:c.const_name$
                   >>)
                constant
              @ [ <:match_case<
                    tag -> Extprot.Error.unknown_tag tag >> ] in

            let nonconstant_match_cases =
              let mc (c, lltys) =
                <:match_case<
                   $int:string_of_int c.const_tag$ ->
                        $f__read_sum_elms msgname constr_name name c
                          (List.map (fun llty -> (llty, default_value `Eager llty)) lltys)$
                >>
              in List.map mc non_constant @
                 [ <:match_case<
                     tag -> Extprot.Error.unknown_tag tag >> ]
            in

            let maybe_match_case (llty, f, l) = match l with
                [] | [_] (* catch-all *)-> None
              | l ->
                  let expr =
                    <:expr< match Extprot.Codec.ll_tag t with [ $Ast.mcOr_of_list l$ ] >>
                  in
                    Some <:match_case< $patt_of_ll_type llty$ -> $f expr$ >> in

            let wrap_non_constant e =
              <:expr<
                let len = $f__reader_func `Read_vint$ s in
                let eot' = $f__reader_func `Offset$ s len in
                let nelms = $f__reader_func `Read_vint$ s in
                let v = $e$ in begin
                  $f__reader_func `Skip_to$ s eot';
                  v
                end >> in

            let match_cases =
              List.filter_map maybe_match_case
                [
                  Codec.Enum, (fun e -> e), constant_match_cases;
                  Codec.Tuple, wrap_non_constant, nonconstant_match_cases;
                ] in

            let bad_type_case =
              <:match_case< ll_type -> Extprot.Error.bad_wire_type ~ll_type () >> in

            let other_cases = match non_constant with
                (c, [ty]) :: _ -> begin match f__raw_rd_func ty with
                    Some (mc, reader_expr) ->
                      <:match_case<
                          $mc$ -> $uid:String.capitalize c.const_type$.$uid:c.const_name$ ($reader_expr$ s)
                        | $bad_type_case$
                      >>
                  | None -> bad_type_case
                end
              | (c, ty :: tys) :: _ -> begin match f__raw_rd_func ty with
                    Some (mc, reader_expr) -> begin match maybe_all (default_value `Eager) tys with
                        None -> bad_type_case
                      | Some defs ->
                          <:match_case<
                              $mc$ ->
                                 $uid:String.capitalize c.const_type$.$uid:c.const_name$
                                   ($reader_expr$ s, $Ast.exCom_of_list defs$)
                            | $bad_type_case$
                          >>
                    end
                  | None -> bad_type_case
                end
              | _ -> bad_type_case in

            let expr =
              <:expr< let t = $f__reader_func `Read_prefix$ s in
                match Extprot.Codec.ll_type t with [
                  $Ast.mcOr_of_list match_cases$
                  | $other_cases$
                ]
              >>
            in
              match ev_regime with
                | `Eager ->
                    update_path_if_needed ~name ~fieldno llty @@
                    wrap expr
                | `Lazy_Immediate ->
                    update_path_if_needed ~name ~fieldno llty
                      <:expr< EXTPROT_FIELD____.from_val $wrap expr$ >>
                | `Lazy ->
                    <:expr<
                      let t   = $RD.reader_func `Read_prefix$ s in
                        match Extprot.Codec.ll_type t with [
                           Extprot.Codec.Enum ->
                              EXTPROT_FIELD____.from_val @@
                              $wrap
                                 <:expr<
                                    match Extprot.Codec.ll_tag t with [
                                      $Ast.mcOr_of_list constant_match_cases$
                                    ]
                                 >>$
                          | _ ->
                              let s = $RD.reader_func `Get_value_reader_with_prefix$ s t in
                              let path = EXTPROT_FIELD____.Hint_path.append_field path $str:name$ $int:string_of_int fieldno$ in
                                EXTPROT_FIELD____.from_fun ?hint ~level ~path s
                                  (fun s -> $wrap expr$)
                        ]
                    >>
          end

        | `Eager, (Record (name, fields, opts) as llty) ->
            let fields' =
              List.map
                (fun f ->
                   (f.field_name, true, compute_field_ev_regime f, f.field_opts, f.field_type))
                fields in
            let promoted_match_cases =
              make_promoted_match_cases msgname [ Some name, None, fields' ] in
            let match_cases =
              record_case_inlined
                ~locs:(use_locs opts) ~namespace:name msgname 0 fields'
            in
              update_path_if_needed ~name ~fieldno llty @@
              wrap @@
              wrap_msg_reader name ~promoted_match_cases match_cases

        | `Lazy, (Record (name, fields, opts) as llty) when deserialize_eagerly llty ->
            let fields' =
              List.map
                (fun f ->
                   (f.field_name, true, compute_field_ev_regime f, f.field_opts, f.field_type))
                fields in
            let promoted_match_cases =
              make_promoted_match_cases msgname [ Some name, None, fields' ] in
            let match_cases =
              record_case_inlined
                ~locs:(use_locs opts) ~namespace:name msgname 0 fields'
            in
              update_path_if_needed ~name ~fieldno llty @@
              <:expr<
                EXTPROT_FIELD____.from_val
                  $wrap @@ wrap_msg_reader name ~promoted_match_cases match_cases$
              >>

        | `Lazy, Record (name, fields, opts) ->
            let fields' =
              List.map
                (fun f -> (f.field_name, true, compute_field_ev_regime f, f.field_opts, f.field_type))
                fields in

            let promoted_match_cases =
              STRING_READER.make_promoted_match_cases msgname [ Some name, None, fields' ] in
            let match_cases =
              STRING_READER.record_case_inlined
                ~locs:(use_locs opts) ~namespace:name msgname 0 fields'
            in
              <:expr<
                let s = $RD.reader_func `Get_value_reader$ s in
                let path = EXTPROT_FIELD____.Hint_path.append_field path $str:name$ $int:string_of_int fieldno$ in
                  EXTPROT_FIELD____.from_fun ?hint ~level ~path s
                    (fun s -> $wrap @@ STRING_READER.wrap_msg_reader name ~promoted_match_cases match_cases$)
                >>

        | `Eager, (Message (path, name, _, _) as llty) ->
            let full_path = path @ [String.capitalize name] in
            let id = ident_with_path _loc full_path (RD.read_msg_func name) in
              update_path_if_needed ~name ~fieldno llty @@
              wrap @@
              if may_use_hint_path llty then
                <:expr< $id:id$ ?hint ~level ~path s >>
              else
                <:expr< $id:id$ s >>

        | `Lazy, (Message (path, name, _, _) as llty) when deserialize_eagerly llty ->
            let full_path = path @ [String.capitalize name] in
            let id = ident_with_path _loc full_path (RD.read_msg_func name) in
              update_path_if_needed ~name ~fieldno llty @@
              wrap @@
              if may_use_hint_path llty then
                <:expr< EXTPROT_FIELD____.from_val ($id:id$ ?hint ~level ~path s) >>
              else
                <:expr< EXTPROT_FIELD____.from_val ($id:id$ s) >>

        | `Lazy, (Message (path, name, _, _) as llty) ->
            let full_path = path @ [String.capitalize name] in
            (* We hardcode use of read_ because it's the String_reader version
             * we want (since it will be operating on the string_reader passed
             * to the thunk. *)
            let id = ident_with_path _loc full_path ("read_" ^ name) in
              update_path_if_needed ~name ~fieldno llty @@
              <:expr<
                let s = $RD.reader_func `Get_value_reader$ s in
                  EXTPROT_FIELD____.from_fun ?hint ~level ~path s
                    (fun s -> $wrap <:expr< $id:id$ s >>$)
              >>

        | ev_regime, Htuple (kind, llty, _) -> begin
            let rd_func = match ev_regime with
              | `Eager -> read
              | `Lazy -> STRING_READER.read in

            let e = match kind with
                | List ->
                  let loop = new_lid "loop" in
                    <:expr<
                      let rec $lid:loop$ acc = fun [
                          0 -> List.rev acc
                        | n ->
                           let v =
                             $rd_func msgname constr_name name
                                ~fieldno ~ev_regime:`Eager llty$
                           in $lid:loop$ [v :: acc] (n - 1)
                      ] in $lid:loop$ [] nelms
                    >>
                | Array ->
                    <:expr<
                      Array.init nelms
                        (fun _ ->
                          $rd_func msgname constr_name name
                            ~fieldno ~ev_regime:`Eager llty$)
                    >> in

            let empty_htuple = match kind with
              | List -> <:expr< [] >>
              | Array -> <:expr< [| |] >>
            in
              match ev_regime with
                | `Eager ->
                    update_path_if_needed ~name ~fieldno llty @@
                    wrap @@
                    <:expr<
                        let t = $RD.reader_func `Read_prefix$ s in
                          match Extprot.Codec.ll_type t with [
                              Extprot.Codec.Htuple ->
                                let len = $RD.reader_func `Read_vint$ s in
                                let eoht = $RD.reader_func `Offset$ s len in
                                let nelms = $RD.reader_func `Read_vint$ s in
                                let () = Extprot.Limits.check_message_length len in
                                let () = Extprot.Limits.check_num_elements nelms in
                                let v = $e$ in begin
                                  $RD.reader_func `Skip_to$ s eoht;
                                  v
                                end
                            | ty -> Extprot.Error.bad_wire_type ~ll_type:ty ()
                          ]
                      >>
                | `Lazy ->

                    let from_fun_expr =
                      match RD.get_string_subreader with
                        | None ->
                            <:expr<
                              let dat = $RD.reader_func `Read_serialized_data$ s len in
                              let ()  = $RD.reader_func `Skip_to$ s eoht in
                              let b   =
                                Extprot.Msg_buffer.make
                                  (if nelms < 128 && len < 128 then 3 + len
                                   else 8 + len)
                              in
                                do {
                                  Extprot.Msg_buffer.add_vint b t;
                                  Extprot.Msg_buffer.add_vint b len;
                                  Extprot.Msg_buffer.add_vint b nelms;
                                  Extprot.Msg_buffer.add_string b dat;
                                  EXTPROT_FIELD____.from_fun ?hint ~level ~path
                                    (Extprot.Reader.String_reader.unsafe_from_msgbuffer b)
                                    (fun s -> $wrap e$)
                                }
                              >>
                        | Some f ->
                            <:expr<
                              do {
                                $RD.reader_func `Skip_to$ s eoht;
                                EXTPROT_FIELD____.from_fun ?hint ~level ~path
                                  (try ($f$ s ~off:boht ~upto:eoht)
                                   with _ ->
                                     Extprot.Error.limit_exceeded
                                       ~message:$str:msgname$
                                       ~constructor:$str:constr_name$
                                       ~field:$str:name$
                                       (Extprot.Error.Message_length
                                         (Extprot.Reader.String_reader.range_length boht eoht)))
                                  (fun s ->
                                    let _ = $STR_OPS.reader_func `Read_prefix$ s in
                                    (* size *)
                                    let _ = $STR_OPS.reader_func `Read_vint$ s in
                                    (* nelms *)
                                    let _ = $STR_OPS.reader_func `Read_vint$ s in
                                      $wrap e$)
                              }
                              >>
                    in
                      <:expr<
                          let boht = $RD.reader_func `Offset$ s 0 in
                          let t    = $RD.reader_func `Read_prefix$ s in
                            match Extprot.Codec.ll_type t with [
                                Extprot.Codec.Htuple ->
                                  let len = $RD.reader_func `Read_vint$ s in
                                  let eoht = $RD.reader_func `Offset$ s len in
                                  let nelms = $RD.reader_func `Read_vint$ s in
                                  let () = Extprot.Limits.check_message_length len in
                                  let () = Extprot.Limits.check_num_elements nelms in
                                    if nelms = 0 then do {
                                      ignore boht;
                                      $RD.reader_func `Skip_to$ s eoht;
                                      EXTPROT_FIELD____.from_val $wrap empty_htuple$
                                    } else begin
                                      let path = EXTPROT_FIELD____.Hint_path.append_field path $str:name$ $int:string_of_int fieldno$ in
                                        $from_fun_expr$
                                    end
                              | ty -> Extprot.Error.bad_wire_type ~ll_type:ty ()
                            ]
                        >>
          end

  and read_field msgname constr_name name ~ev_regime ~fieldno llty =
    let _loc = loc "<generated code @ read_field>" in
      read msgname constr_name name ~ev_regime ~fieldno llty

  and field_reader_funcname ?(ev_regime=`Eager) ~msgname ~constr ~name () =
    let mangle s =
      let b = Buffer.create 13 in
        String.iter
          (function
            | '_' -> Buffer.add_string b "__"
            | c -> Buffer.add_char b c)
          s;
        Buffer.contents b
    in
      match ev_regime with
        | `Eager ->
            sprintf "__%s_read_x%s_x%s_x%s"
              RD.name (mangle msgname) (Option.map_default mangle "" constr) (mangle name)
        | `Lazy ->
            sprintf "__%s_readL_x%s_x%s_x%s"
              RD.name (mangle msgname) (Option.map_default mangle "" constr) (mangle name)

  and record_case_field_readers opts msgname ~field_reader_func_uses ~locs ?constr _ fields =
    let _loc = Loc.mk "<generated code @ record_case_field_readers>" in
    let constr_name = Option.default "<default>" constr in

    let read_field_with_locs fieldno (name, _, ev_regime, _, llty) =
      let ev_regime = compute_ev_regime_with_llty llty ev_regime in

      let rescue_match_case = match default_value ev_regime llty with
          None ->
            <:match_case<
              Extprot.Error.Extprot_error (e, loc) ->
                Extprot.Error.failwith_location
                  ~message:$str:msgname$
                  ~constructor:$str:constr_name$
                  ~field:$str:name$
                  e loc
            >>
        | Some _expr ->
            <:match_case<
                Extprot.Error.Extprot_error
                  ((Extprot.Error.Bad_format (Extprot.Error.Bad_wire_type _) as e), loc) ->
                    Extprot.Error.failwith_location
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      e loc >> in
      let default = match default_value ev_regime llty with
        | Some expr -> expr
        | None ->
            <:expr< Extprot.Error.missing_field
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      ()
            >> in

      let funcname  = field_reader_funcname ~ev_regime ~msgname ~constr ~name () in

        match ev_regime with
        | `Lazy when
            deserialize_eagerly llty &&
            (* cannot reuse eager field reader func if it's not emitted! *)
            is_field_reader_func_used opts field_reader_func_uses msgname name `Eager llty ->
            let eagerfunc = field_reader_funcname ~ev_regime:`Eager ~msgname ~constr ~name () in
              <:str_item<
                value $lid:funcname$ ?hint ?level ?path s nelms =
                  let _ = hint in
                  let _ = level in
                  let _ = path in
                    EXTPROT_FIELD____.from_val ($lid:eagerfunc$ s nelms)
              >>

        | _ ->
            let read_expr =
              if locs then
                <:expr<
                  try
                    $read_field msgname constr_name name ~ev_regime ~fieldno llty$
                  with [$rescue_match_case$]
                >>
              else
                read_field msgname constr_name name ~ev_regime ~fieldno llty
            in
              <:str_item<
                value $lid:funcname$ ?hint ?(level = 0) ?(path = EXTPROT_FIELD____.Hint_path.null) s nelms =
                  let _ = hint in
                  let _ = level in
                  let _ = path in
                    if nelms >= $int:string_of_int (fieldno + 1)$ then
                      $read_expr$
                    else $default$
              >> in

    let fields_with_index =
      List.concat @@
      List.mapi
        (fun i (name, mut, _, fopts, llty) ->
           let used evr =
             is_field_reader_func_used opts field_reader_func_uses msgname name evr llty
           in
             List.concat
               [ if used `Eager then [(i, (name, mut, `Eager, fopts, llty))] else [];
                 if used `Lazy then [(i, (name, mut, `Lazy, fopts, llty))] else [];
               ])
        fields
    in
      List.fold_right
        (fun (i, fieldinfo) functions ->
           <:str_item< $read_field_with_locs i fieldinfo$; $functions$ >>)
        fields_with_index
        <:str_item< >>

  and is_field_reader_func_used opts field_reader_func_uses msgname name evr llty =
    let assumed = assumed_subsets opts in
      (List.mem "all" assumed || List.mem (String.capitalize msgname) assumed) ||
      List.exists (fun (_, evr') -> compute_ev_regime_with_llty llty evr = compute_ev_regime_with_llty llty evr') @@
      smap_find_default name [] @@
      smap_find_default msgname SMap.empty field_reader_func_uses

  and record_case_inlined msgname ~locs ?namespace ?constr tag fields =
    let _loc        = Loc.mk "<generated code @ record_case_inlined>" in
    let constr_name = Option.default "<default>" constr in

    let read_field_with_locs fieldno (name, _, ev_regime, _, llty) expr =

      let ev_regime = compute_ev_regime_with_llty llty ev_regime in

      let rescue_match_case = match default_value ev_regime llty with
          None ->
            <:match_case<
              Extprot.Error.Extprot_error (e, loc) ->
                Extprot.Error.failwith_location
                  ~message:$str:msgname$
                  ~constructor:$str:constr_name$
                  ~field:$str:name$
                  e loc
            >>
        | Some _expr ->
            <:match_case<
                Extprot.Error.Extprot_error
                  ((Extprot.Error.Bad_format (Extprot.Error.Bad_wire_type _) as e), loc) ->
                    Extprot.Error.failwith_location
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      e loc >> in
      let default = match default_value ev_regime llty with
          Some expr -> expr
        | None ->
            <:expr< Extprot.Error.missing_field
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      ()
            >> in

      let read_expr =
        if locs then
          <:expr<
            try
              $read_field msgname constr_name name ~ev_regime ~fieldno llty$
            with [$rescue_match_case$]
          >>
        else
          read_field msgname constr_name name ~ev_regime ~fieldno llty
      in
        <:expr<
           let $lid:name$ =
             if nelms >= $int:string_of_int (fieldno + 1)$ then
               $read_expr$
             else
                 $default$
           in $expr$
        >> in

    let field_assigns =
      List.map
        (fun (name, _, _ev_regime, _, _) -> match namespace with
             None -> <:rec_binding< $lid:name$ = $lid:name$ >>
           | Some ns -> <:rec_binding< $uid:String.capitalize ns$.$lid:name$ = $lid:name$ >>)
        fields in
    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in
    let e =
      List.fold_right
        (fun (i, fieldinfo) e -> read_field_with_locs i fieldinfo e)
        (List.mapi (fun i x -> (i, x)) fields)
        record
    in
      if not @@ List.exists (fun (_, _, _, _, llty) -> may_use_hint_path llty) fields then
        <:match_case<
          $int:string_of_int tag$ ->
            try
              let v = $e$ in begin
                $RD.reader_func `Skip_to$ s eom;
                v
              end
            with [ e -> begin $RD.reader_func `Skip_to$ s eom; raise e end ]
        >>
      else
        <:match_case<
          $int:string_of_int tag$ ->
            let path = EXTPROT_FIELD____.Hint_path.append_constr path $str:constr_name$ $int:string_of_int tag$ in
            let _    = path in
              try
                let v = $e$ in begin
                  $RD.reader_func `Skip_to$ s eom;
                  v
                end
              with [ e -> begin $RD.reader_func `Skip_to$ s eom; raise e end ]
        >>

  and record_case msgname ~locs:_ ?namespace ?constr tag fields =
    let _loc = Loc.mk "<generated code @ record_case>" in

    let read_field_using_func _ (name, _, ev_regime, _, llty) expr =

      let ev_regime = compute_ev_regime_with_llty llty ev_regime in

      let funcname = field_reader_funcname ~msgname ~constr ~name () in
        match ev_regime with
          | `Eager when not @@ may_use_hint_path llty ->
              <:expr<
                let $lid:name$ = $lid:funcname$ s nelms in
                  $expr$
              >>
          | `Eager ->
              <:expr<
                let $lid:name$ = $lid:funcname$ ?hint ~level ~path s nelms in
                  $expr$
              >>
          | `Lazy ->
              let funcname = field_reader_funcname ~ev_regime:`Lazy ~msgname ~constr ~name () in
                if may_use_hint_path llty then
                  <:expr<
                    let $lid:name$ = ($lid:funcname$ ?hint ~level ~path s nelms) in
                      $expr$
                  >>
                else
                  <:expr<
                    let $lid:name$ = ($lid:funcname$ s nelms) in
                      $expr$
                  >>
    in

    let field_assigns =
      List.map
        (fun (name, _, _ev_regime, _, _) -> match namespace with
             None -> <:rec_binding< $lid:name$ = $lid:name$ >>
           | Some ns -> <:rec_binding< $uid:String.capitalize ns$.$lid:name$ = $lid:name$ >>)
        fields in
    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in
    let e =
      List.fold_right
        (fun (i, fieldinfo) e -> read_field_using_func i fieldinfo e)
        (List.mapi (fun i x -> (i, x)) fields)
        record
    in
      if not @@ List.exists (fun (_, _, _, _, llty) -> may_use_hint_path llty) fields then
        <:match_case<
          $int:string_of_int tag$ ->
            try
              let v = $e$ in begin
                $RD.reader_func `Skip_to$ s eom;
                v
              end
            with [ e -> begin $RD.reader_func `Skip_to$ s eom; raise e end ]
        >>
      else
        <:match_case<
          $int:string_of_int tag$ ->
            let path =
              EXTPROT_FIELD____.Hint_path.append_constr path
                $str:Option.default "<default>" constr$ $int:string_of_int tag$  in
            let _    = path in
              try
                let v = $e$ in begin
                  $RD.reader_func `Skip_to$ s eom;
                  v
                end
              with [ e -> begin $RD.reader_func `Skip_to$ s eom; raise e end ]
        >>

  and subset_case ~locs ~orig msgname ?namespace ?constr tag fields ~subset =
    let _loc        = Loc.mk "<generated code @ subset_case>" in
    let constr_name = Option.default "<default>" constr in

    let read_field_with_locs_if_kept fieldno ((name, _, _, _, _) as field) expr =
      match
        match must_keep_field subset field with
          | None -> None
          | Some (`Orig (name, mut, evr, fopts, llty)) ->
              Some (`Orig (name, mut, compute_ev_regime_with_llty llty evr, fopts, llty))
          | Some (`Newtype (name, mut, evr, fopts, llty)) ->
              Some (`Newtype (name, mut, compute_ev_regime_with_llty llty evr, fopts, llty))
      with
        | None ->
            <:expr<
              let () =
                if nelms >= $int:string_of_int (fieldno + 1)$ then
                  ignore ($RD.reader_func `Skip_value$ s ($RD.reader_func `Read_prefix$ s))
                else ()
              in
                $expr$
            >>
        | Some (`Orig (_, _, `Eager, _, llty)) when not @@ may_use_hint_path llty ->
            (* We avoid unnecessary work updating the path if we know it won't
             * be used (because the field type is a primitive or another type
             * we can statically determine not to have lazy components). *)
            let funcname = field_reader_funcname ~msgname:orig ~constr ~name () in
              <:expr<
                let $lid:name$ =
                  $uid:String.capitalize orig$.$lid:funcname$ s nelms
                in
                  $expr$
              >>
        | Some (`Orig (_, _, `Eager, _, _)) ->
            let funcname = field_reader_funcname ~msgname:orig ~constr ~name () in
              <:expr<
                let $lid:name$ =
                  let path = EXTPROT_FIELD____.Hint_path.append_field path $str:name$ $int:string_of_int fieldno$ in
                  let _    = path in
                    $uid:String.capitalize orig$.$lid:funcname$ ?hint ~level ~path s nelms
                in
                  $expr$
              >>
        | Some (`Orig (_, _, `Lazy, _, llty)) ->
            let funcname = field_reader_funcname ~ev_regime:`Lazy ~msgname:orig ~constr ~name () in
              if may_use_hint_path llty then
                <:expr<
                  let $lid:name$ =
                    let path = EXTPROT_FIELD____.Hint_path.append_field path $str:name$ $int:string_of_int fieldno$ in
                    let _    = path in
                      $uid:String.capitalize orig$.$lid:funcname$ ?hint ~level ~path s nelms
                  in
                    $expr$
                >>
              else
                <:expr<
                  let $lid:name$ = $uid:String.capitalize orig$.$lid:funcname$ s nelms
                  in
                    $expr$
                >>
        | Some (`Newtype (name, _, ev_regime, _, llty)) ->
            let ev_regime = compute_ev_regime_with_llty llty ev_regime in

            let rescue_match_case = match default_value ev_regime llty with
                None ->
                  <:match_case<
                    Extprot.Error.Extprot_error (e, loc) ->
                      Extprot.Error.failwith_location
                        ~message:$str:msgname$
                        ~constructor:$str:constr_name$
                        ~field:$str:name$
                        e loc
                  >>
              | Some _expr ->
                  <:match_case<
                      Extprot.Error.Extprot_error
                        ((Extprot.Error.Bad_format (Extprot.Error.Bad_wire_type _) as e), loc) ->
                          Extprot.Error.failwith_location
                            ~message:$str:msgname$
                            ~constructor:$str:constr_name$
                            ~field:$str:name$
                            e loc >> in
            let default = match default_value ev_regime llty with
                Some expr -> expr
              | None ->
                  <:expr< Extprot.Error.missing_field
                            ~message:$str:msgname$
                            ~constructor:$str:constr_name$
                            ~field:$str:name$
                            ()
                  >> in

            let read_expr =
              if locs then
                <:expr<
                  try
                    $read_field msgname constr_name name ~ev_regime ~fieldno llty$
                  with [$rescue_match_case$]
                >>
              else
                read_field msgname constr_name name ~ev_regime ~fieldno llty
            in
              if not @@ may_use_hint_path llty then
                <:expr<
                   let $lid:name$ =
                     if nelms >= $int:string_of_int (fieldno + 1)$ then begin
                       $read_expr$
                     end else $default$
                   in $expr$
                >>
              else
                <:expr<
                   let $lid:name$ =
                     if nelms >= $int:string_of_int (fieldno + 1)$ then begin
                       let path = EXTPROT_FIELD____.Hint_path.append_field path $str:name$ $int:string_of_int fieldno$ in
                       let _    = path in
                         $read_expr$
                     end else $default$
                   in $expr$
                >>
    in

    let field_assigns =
      List.map
        (fun (name, _, _ev_regime, _fopts, _) -> match namespace with
             None -> <:rec_binding< $lid:name$ = $lid:name$ >>
           | Some ns -> <:rec_binding< $uid:String.capitalize ns$.$lid:name$ = $lid:name$ >>) @@
      List.map subset_field @@
      List.filter_map (must_keep_field subset) fields in

    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in
    let e =
      List.fold_right
        (fun (i, fieldinfo) e -> read_field_with_locs_if_kept i fieldinfo e)
        (List.mapi (fun i x -> (i, x)) @@
         List.fold_right
           (fun ((_name, _, _, _, _) as f) fs -> match fs with
              | _ :: _ -> f :: fs
              | [] -> if Option.is_some @@ must_keep_field subset f then f :: fs else [])
           fields [])
        record
    in

      (* check that the subset is not empty *)
      begin
        match List.filter_map (must_keep_field subset) fields with
          | _ :: _ -> ()
          | [] -> exit_with_error "Message subset %s is empty." msgname
      end;

      <:match_case<
        $int:string_of_int tag$ ->
          try
            let v = $e$ in begin
              $RD.reader_func `Skip_to$ s eom;
              v
            end
          with [ e -> begin $RD.reader_func `Skip_to$ s eom; raise e end ]
      >>

  and wrap_msg_reader msgname ?promoted_match_cases match_cases =
    let _loc       = Loc.mk "<generated code @ wrap_msg_reader>" in
    let promo_expr =
      match promoted_match_cases with
        | Some promoted_match_cases ->
            <:expr<
              let raise_bad_wire_type () =
                 Extprot.Error.bad_wire_type
                   ~message:$str:msgname$ ~ll_type:(Extprot.Codec.ll_type t) ()
              in
                match Extprot.Codec.ll_tag t with [
                  $promoted_match_cases$
                | tag -> Extprot.Error.unknown_tag ~message:$str:msgname$ tag
                ]
            >>
        | None ->
            <:expr<
              Extprot.Error.bad_wire_type
                ~message:$str:msgname$ ~ll_type:(Extprot.Codec.ll_type t) ()
            >>
    in
      <:expr<
        let t = $RD.reader_func `Read_prefix$ s in begin
          if Extprot.Codec.ll_type t = Extprot.Codec.Tuple then
            let len   = $RD.reader_func `Read_vint$ s in
            let eom   = $RD.reader_func `Offset$ s len in
            let nelms = $RD.reader_func `Read_vint$ s in
            let () = Extprot.Limits.check_num_elements ~message:$str:msgname$ nelms in
            let () = Extprot.Limits.check_message_length ~message:$str:msgname$ len in
              match Extprot.Codec.ll_tag t with [
                $match_cases$
                | tag -> Extprot.Error.unknown_tag ~message:$str:msgname$ tag
              ]
          else $promo_expr$
        end
      >>

  and promoted_type_reader ?namespace ?constr msgname fields =
    let constr_name = Option.default "<default>" constr in

    let first_field_expr =
      match fields with
          [] -> failwith (sprintf "no fields in msg %s" msgname)
        | (_field_name, _, ev_regime, _fopts, field_type) :: _ ->
            match compute_ev_regime_with_llty field_type ev_regime, RD.raw_rd_func field_type with
              | _, None -> <:expr< raise_bad_wire_type () >>
              | `Eager, Some (mc, reader_expr) ->
                <:expr<
                  match Extprot.Codec.ll_type t with [
                      $mc$ -> $reader_expr$ s
                    | _ -> raise_bad_wire_type ()
                  ]
                >>
              | `Lazy, Some (mc, reader_expr) ->
                <:expr<
                  match Extprot.Codec.ll_type t with [
                      $mc$ -> EXTPROT_FIELD____.from_val ($reader_expr$ s)
                    | _ -> raise_bad_wire_type ()
                  ]
                >>
    in

    let other_field_readers =
      match fields with
          [] | [_] -> []
        | _ :: others ->
            List.map
              (fun (name, _, ev_regime, _, llty) ->
                 match default_value ev_regime llty with
                   | Some expr -> expr
                   | None ->
                       <:expr< Extprot.Error.missing_field
                                 ~message:$str:msgname$
                                 ~constructor:$str:constr_name$
                                 ~field:$str:name$
                                 ()
                       >>)
              others in

    let field_assigns =
      List.mapi
        (fun i (name, _, _ev_regime, _fopts, _) -> match namespace with
             None -> <:rec_binding< $lid:name$ = $lid:sprintf "v%d" i$ >>
           | Some ns -> <:rec_binding< $uid:String.capitalize ns$.$lid:name$ =
                                          $lid:sprintf "v%d" i$ >>)
        fields in
    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in

    let read_others_and_assign =
      List.fold_right
        (fun (idx, read_expr) e ->
           <:expr<
             let $lid:sprintf "v%d" idx$ = $read_expr$ in $e$ >>)
        (List.mapi (fun i r -> (i + 1, r)) other_field_readers)
        record
    in
      <:expr<
         let v0 = $first_field_expr$ in
           $read_others_and_assign$ >>

  and make_promoted_match_cases msgname field_infos =
    List.mapi
      (fun tag (namespace, constr, (fields : field_info list)) ->
         <:match_case<
           $int:string_of_int tag$ ->
             $promoted_type_reader ?namespace ?constr msgname fields$ >>)
      field_infos |>
    Ast.mcOr_of_list

  and read_message ?(inline = false) ~field_reader_func_uses msgname opts = function
      | Message_single (namespace, fields) ->

          let promoted_match_cases =
            make_promoted_match_cases msgname [ namespace, None, fields ] in

          let match_cases, field_readers =
            if inline then
              let match_cases =
                record_case_inlined ~locs:(use_locs opts) ?namespace msgname 0 fields
              in
                (match_cases, <:str_item< >>)
            else
              let locs          = use_locs opts in
              let match_cases   = record_case ~locs:(use_locs opts) ?namespace msgname 0 fields in
              let field_readers = record_case_field_readers opts ~field_reader_func_uses ~locs msgname 0 fields in
                (match_cases, field_readers) in

          let main_expr =
            wrap_msg_reader msgname ~promoted_match_cases match_cases |>
            wrap_reader opts
          in
            (field_readers, main_expr)

      | Message_subset (orig, fields, subset) ->
          let match_cases = subset_case ~locs:(use_locs opts) ~orig msgname 0 fields ~subset in
          let main_expr = wrap_msg_reader msgname match_cases |> wrap_reader opts in
            (<:str_item< >>, main_expr)

      | Message_alias (path, name) ->
          let full_path = path @ [String.capitalize name] in
          let _loc = Loc.mk "<generated code @ read_message>" in
          let reader_func = RD.read_msg_func name in
          let main_expr =
            wrap_reader opts
            <:expr< $id:ident_with_path _loc full_path reader_func$ s >>
          in
            (<:str_item< >>, main_expr)

      | Message_typealias (name, _) ->
          let full_path = [String.capitalize name] in
          let _loc = Loc.mk "<generated code @ read_message>" in
          let reader_func = RD.read_msg_func name in
          let main_expr =
            wrap_reader opts
            <:expr< $id:ident_with_path _loc full_path reader_func$ s >>
          in
            (<:str_item< >>, main_expr)

      | Message_sum l ->
          let promoted_match_cases =
            make_promoted_match_cases msgname
              (List.map
                 (fun (ns, constr, fields) ->
                    (Some (Option.default constr ns), Some constr, fields))
                 l) in

          let locs    = use_locs opts in
          let inlined = true in

          let field_readers =
            if inlined then
              <:str_item< >>
            else
              List.fold_left
                (fun funcs func ->
                   <:str_item< $func$; $funcs$ >>)
                <:str_item< >> @@
              List.rev @@
              List.mapi
                (fun tag (_namespace, constr, fields) ->
                   record_case_field_readers opts ~field_reader_func_uses ~locs ~constr msgname tag fields)
                l in

          let match_cases =
            let _record_case =
              if inlined then
                record_case_inlined
              else
                record_case
            in
              List.mapi
                (fun tag (namespace, constr, fields) ->
                   _record_case
                     ~locs ~namespace:(Option.default constr namespace)
                     msgname ~constr tag fields) l
              |> Ast.mcOr_of_list in

          let main_expr =
            wrap_msg_reader msgname ~promoted_match_cases match_cases |>
            wrap_reader opts
          in
            (field_readers, main_expr)
end

let messages_with_subsets opts bindings =

  let assume_subsets = assumed_subsets opts in
  let assume_all = List.mem "all" assume_subsets in

    SMap.fold
      (fun _ decl l -> match decl with
         | Message_decl (_, `Message_subset (name, _, _), _, _) when
             assume_all || List.mem (String.capitalize name) assume_subsets ->
             name :: l
         | Message_decl (_, `Message_subset (name, _, _), _, _) -> begin
             let rec find_base_msgname name =
               match smap_find name bindings with
                 | Some (Message_decl (_, `Message_app (name, [], _), _, _)) ->
                     find_base_msgname name
                 | _ -> name
             in
               find_base_msgname name :: l
           end
         | Message_decl
             (n, (`Message_record _ | `Message_alias _ |
                  `Message_sum _ | `Message_app _), _, _)
         | Type_decl (n, _, _, _) ->
             if assume_all || List.mem (String.capitalize n) assume_subsets
             then n :: l else l)
      bindings []

let field_reader_func_uses bindings =
  let rec update m msgname = function
    (* m: map   msgname -> fieldname -> (msgname * ev_regime) list *)
    | `Message_record fs as mexpr ->
        let ev_regimes =
          match Gencode.low_level_msg_def bindings msgname mexpr with
            | Message_single (_, fs) ->
                List.fold_left
                  (fun m (name, _, evr, _, llty) ->
                     SMap.add name (compute_ev_regime_with_llty llty evr) m)
                  SMap.empty fs
            | _ -> failwith "low level msg def for `Message_record is not Message_single"
        in
          smap_update_default
            (fun m2 ->
               List.fold_left
                 (fun m (fname, _, _, _, _) ->
                    smap_update_default
                      (fun l -> (msgname, SMap.find fname ev_regimes) :: l) fname [] m)
                 m2 fs)
            msgname SMap.empty m
    | `Message_alias _ -> m
    | `Message_sum cases ->
        List.fold_left
          (fun m (constr, mexpr) -> match mexpr with
             | `Message_app _ | `Message_record _ as mexpr ->
                 update m (msgname ^ "." ^ constr) mexpr)
          m cases
    | `Message_app (tyname, _, _) as mexpr -> begin
        match smap_find tyname bindings with
          | Some (Type_decl (_, _, `Record (r, _), _)) ->
              let ev_regimes =
                match Gencode.low_level_msg_def bindings msgname mexpr with
                  | Message_single (_, fs) ->
                      List.fold_left
                        (fun m (name, _, evr, _, llty) ->
                           SMap.add name (compute_ev_regime_with_llty llty evr) m)
                        SMap.empty fs
                  | _ -> failwith "low level msg def for `Message_record is not Message_single"
              in
                smap_update_default
                  (fun m ->
                     List.fold_left
                       (fun m (fname, _, _, _, _) ->
                          smap_update_default
                            (fun l -> (msgname, SMap.find fname ev_regimes) :: l) fname [] m)
                       m r.record_fields)
                  msgname SMap.empty m
          | _ -> m
      end
    | `Message_subset _ as mexpr ->
        match Gencode.low_level_msg_def bindings msgname mexpr with
          | Message_subset (origname, fs, subset) ->
              smap_update_default
                (fun m2 ->
                   List.fold_left
                     (fun m2 f ->
                        match must_keep_field subset f with
                          | None -> m2
                          | Some (`Newtype (fname, _, fevr, _, llty))
                          | Some (`Orig (fname, _, fevr, _, llty)) ->
                              smap_update_default
                                (fun l ->
                                   (msgname, compute_ev_regime_with_llty llty fevr) :: l) fname [] m2)
                     m2 fs)
                origname SMap.empty m
          | _ -> m
  in
    SMap.fold
      (fun _ decl m -> match decl with
         | Message_decl (msgname, mexpr, _, _) -> update m msgname mexpr
         | Type_decl _ -> m)
      bindings SMap.empty

let only_if_export opts export s = match export with
  | true -> Some s
  | _ when List.mem_assoc "export_tys" opts -> Some s
  | false -> None

let add_message_reader ~export bindings msgname mexpr opts c =
  let _loc = Loc.mk "<generated code @ add_message_reader>" in
  let llrec = Gencode.low_level_msg_def bindings msgname mexpr in
  let module Mk_normal_reader = Make_reader(STR_OPS) in

  let with_subsets = messages_with_subsets opts bindings in

  let field_readers, read_expr =
    Mk_normal_reader.read_message
      ~field_reader_func_uses:(field_reader_func_uses bindings)
      ~inline:(not @@ List.mem msgname with_subsets)
      msgname opts llrec in

  let str_item_read =
    if msg_may_use_hint_path llrec then
      <:str_item<
        value $lid:"read_" ^ msgname$ ?hint ?(level = 0) ?(path = EXTPROT_FIELD____.Hint_path.null) s =
          let path  = EXTPROT_FIELD____.Hint_path.append_type path $str:msgname$ in
          let level = level + 1 in
          let _     = path in
          let _     = level in
            $read_expr$;
        >>
    else
      <:str_item<
        value $lid:"read_" ^ msgname$ ?hint ?level ?path s =
          let _ = hint in
          let _ = level in
          let _ = path in
            $read_expr$;
        >> in

  let field_mod =
    match List.assoc "field-module" opts with
      | exception Not_found -> "Extprot.Field"
      | s -> s
  in
    {
      c with
        c_reader =
          Some <:str_item<
                $field_readers$;
                $str_item_read$;

                value $lid:"io_read_" ^ msgname$ ?hint ?level ?path io =
                  $lid:"read_" ^ msgname$
                    ?hint ?level ?path
                    (Extprot.Reader.String_reader.from_io_reader io);

                value $lid:"fast_io_read_" ^ msgname$ = $lid:"io_read_" ^ msgname$;

                value read = $lid:"read_" ^ msgname$;
                value io_read = $lid:"io_read_" ^ msgname$;
                value fast_io_read = $lid:"fast_io_read_" ^ msgname$;
              >>;

        c_sig_reader =
          only_if_export opts export @@
          begin
            Printf.sprintf
              "val read_%s : (%s, %s.hint, %s.path) Extprot.Conv.string_reader\n\
               val io_read_%s : (%s, %s.hint, %s.path) Extprot.Conv.io_reader\n\
               val fast_io_read_%s : (%s, %s.hint, %s.path) Extprot.Conv.io_reader\n\
               val read : (%s, %s.hint, %s.path) Extprot.Conv.string_reader\n\
               val io_read : (%s, %s.hint, %s.path) Extprot.Conv.io_reader\n\
               val fast_io_read : (%s, %s.hint, %s.path) Extprot.Conv.io_reader\n"
              msgname msgname field_mod field_mod
              msgname msgname field_mod field_mod
              msgname msgname field_mod field_mod
              msgname field_mod field_mod
              msgname field_mod field_mod
              msgname field_mod field_mod
          end
    }

let add_message_io_reader ~export:_ bindings msgname mexpr opts c =
  let _loc = Loc.mk "<generated code @ add_message_io_reader>" in
  let llrec = Gencode.low_level_msg_def bindings msgname mexpr in
  let module Mk_io_reader =
    Make_reader(struct
                  let name = "IO"
                  let reader_func t =
                    <:expr< Extprot.Reader.IO_reader.
                              $lid:Reader.string_of_reader_func t$ >>

                  let get_string_subreader = None

                  let raw_rd_func = raw_rd_func reader_func

                  let read_msg_func = ((^) "io_read_")
                end) in

  let with_subsets = messages_with_subsets opts bindings in

  let field_readers, ioread_expr =
    Mk_io_reader.read_message
      ~field_reader_func_uses:(field_reader_func_uses bindings)
      ~inline:(not @@ List.mem msgname with_subsets)
      msgname opts llrec
  in
    if msg_may_use_hint_path llrec then
      {
        c with c_io_reader =
          Some <:str_item<
                  $field_readers$;
                  value $lid:"io_read_" ^ msgname$
                    ?hint ?(level = 0) ?(path = EXTPROT_FIELD____.Hint_path.null) s =
                    let path  = EXTPROT_FIELD____.Hint_path.append_type path $str:msgname$ in
                    let level = level + 1 in
                    let _     = hint in
                    let _     = path in
                    let _     = level in
                      $ioread_expr$;

                  value io_read = $lid:"io_read_" ^ msgname$;
               >>
      }
    else
      {
        c with c_io_reader =
          Some <:str_item<
                  $field_readers$;
                  value $lid:"io_read_" ^ msgname$ ?hint ?level ?path s =
                    let _ = hint in
                    let _ = level in
                    let _ = path in
                      $ioread_expr$;

                  value io_read = $lid:"io_read_" ^ msgname$;
               >>
      }

let rec write_field ~ev_regime ?namespace fname llty =
  let _loc = Loc.mk "<generated code @ write>" in
  let simple_write_func = function
      Vint (Bool, _) -> "write_bool"
    | Vint (Int, _) -> "write_relative_int"
    | Vint (Int8, _) -> "write_int8"
    | Bitstring32 _ -> "write_int32"
    | Bitstring64 (Long, _) -> "write_int64"
    | Bitstring64 (Float, _) -> "write_float"
    | Bytes _ -> "write_string"
    | Tuple _ | Sum _ | Record _ | Htuple _ | Message _ -> assert false in

  let rec write_tuple_values tag var_tys =
    let nelms = List.length var_tys in
    let write_elms =
      List.map (fun (v, ty) -> write <:expr< $lid:v$ >> ty) var_tys
    in
      <:expr<
        let abuf =
          let aux = Extprot.Msg_buffer.create () in do {
            $Ast.exSem_of_list write_elms$;
            aux
          }
        in do {
          Extprot.Msg_buffer.add_tuple_prefix aux $int:string_of_int tag$;
          Extprot.Msg_buffer.add_vint aux
            (Extprot.Msg_buffer.length abuf +
             $int:string_of_int @@ Codec.vint_length nelms$);
          Extprot.Msg_buffer.add_vint aux $int:string_of_int nelms$;
          Extprot.Msg_buffer.add_buffer aux abuf;
          Extprot.Msg_buffer.discard abuf
        }
      >>

  and write_tuple tag v lltys =
    let var_tys = List.mapi (fun i ty -> (sprintf "v%d" i, ty)) lltys in
    let patt =
      Ast.paCom_of_list @@ List.map (fun (v, _) -> <:patt< $lid:v$ >>) var_tys
    in
      <:expr<
        let $patt$ = $v$ in
          $write_tuple_values tag var_tys$
      >>

  and write v llty = match llty with
    | Vint (_, opts) | Bitstring32 opts | Bitstring64 (_, opts) | Bytes opts ->
        <:expr< Extprot.Msg_buffer.$lid:simple_write_func llty$
                  aux $wrap_value opts v$ >>
    | Message (path, name, _, _) ->
        let full_path = path @ [String.capitalize name] in
        let id = ident_with_path _loc full_path ("write_" ^ name) in
          <:expr< $id:id$ aux $v$ >>
    | Tuple (lltys, opts) -> write_tuple 0 (wrap_value opts v) lltys
    | Htuple (kind, llty, opts) ->
        let iter_f = match kind with
            Array -> <:expr< Array.iter >>
          | List -> <:expr< List.iter >>
        in
          <:expr<
            let write_elm aux v = $write <:expr< v >> llty$ in
            let nelms = ref 0 in
            let abuf = Extprot.Msg_buffer.create () in do {
                $iter_f$ (fun v -> do { write_elm abuf v; incr nelms })
                         $wrap_value opts v$;
                Extprot.Msg_buffer.add_htuple_prefix aux 0;
                Extprot.Msg_buffer.add_vint aux
                  (Extprot.Msg_buffer.length abuf +
                   Extprot.Codec.vint_length nelms.contents);
                Extprot.Msg_buffer.add_vint aux nelms.contents;
                Extprot.Msg_buffer.add_buffer aux abuf;
                Extprot.Msg_buffer.discard abuf
              }
         >>
    | Sum (constructors, opts) ->
        let constant, non_constant = partition_constructors constructors in
        let constant_match_cases =
          List.map
            (fun c ->
               <:match_case<
                 $uid:String.capitalize c.const_type$.$lid:c.const_name$ ->
                   Extprot.Msg_buffer.add_const_prefix aux $int:string_of_int c.const_tag$
               >>)
            constant in
        let non_constant_cases =
          List.map
            (fun (c, lltys) ->
               let var_tys = List.mapi (fun i ty -> (sprintf "v%d" i, ty)) lltys in
               let patt =
                 List.map (fun (v, _) -> <:patt< $lid:v$ >>) var_tys |>
                 Ast.paCom_of_list
               in
                 <:match_case<
                   $uid:String.capitalize c.const_type$.$uid:c.const_name$
                     $patt$ -> $write_tuple_values c.const_tag var_tys$
                 >>)
            non_constant in
        let match_cases = constant_match_cases @ non_constant_cases in
          begin match get_type_info opts with
              None -> <:expr< match $v$ with [ $Ast.mcOr_of_list match_cases$ ] >>
            | Some { tof; _ } ->
                <:expr< match $tof$ $v$ with [ $Ast.mcOr_of_list match_cases$ ] >>
          end

    | Record (name, fields, opts) ->

        let fields' =
          List.map
            (fun f -> (f.field_name, true, compute_field_ev_regime f, f.field_opts, f.field_type))
            fields in
        let v = match get_type_info opts with
            None -> v
          | Some { tof; _ } -> <:expr< $tof$ $v$ >>
        in
          <:expr< let b = aux in
                  let msg = $ v $ in
                    $dump_fields ~namespace:name 0 fields'$ >> in

  let v = match namespace with
      | None -> <:expr< msg.$lid:fname$ >>
      | Some ns -> <:expr< msg.$uid:String.capitalize ns$.$lid:fname$ >>
  in
    match ev_regime with
      | `Eager -> write v llty
      | `Lazy ->
          let force_and_write = write <:expr< EXTPROT_FIELD____.force $v$ >> llty in
            <:expr<
              match EXTPROT_FIELD____.get_reader $v$ with [
                  None -> $force_and_write$
                | Some r -> Extprot.Reader.String_reader.append_to_buffer r aux
              ]
            >>

and write_fields ?namespace fs =
  Ast.exSem_of_list @@
  List.map
    (fun (name, _, ev_regime, _, llty) ->
       let ev_regime = compute_ev_regime_with_llty llty ev_regime in
         write_field ~ev_regime ?namespace name llty) fs

and dump_fields ?namespace tag fields =
  let _loc = Loc.mk "<generated code @ dump_fields>" in
    let nelms = List.length fields in
      <:expr<
         let aux = Extprot.Msg_buffer.create () in
         let nelms = $int:string_of_int nelms$ in do {
           Extprot.Msg_buffer.add_tuple_prefix b  $int:string_of_int tag$;
           $write_fields ?namespace fields$;
           Extprot.Msg_buffer.add_vint b
             (Extprot.Msg_buffer.length aux +
              $int:string_of_int @@ Codec.vint_length nelms$);
           Extprot.Msg_buffer.add_vint b nelms;
           Extprot.Msg_buffer.add_buffer b aux;
           Extprot.Msg_buffer.discard aux
         }
      >>

and write_message msgname msg =
  ignore msgname;
  let _loc = Loc.mk "<generated code @ write_message>" in
    match msg with
      | Message_single (namespace, fields) -> Some (dump_fields ?namespace 0 fields)
      | Message_alias (path, name) ->
          let full_path = path @ [String.capitalize name] in
            Some <:expr< $id:ident_with_path _loc full_path ("write_" ^ name)$ b msg >>
      | Message_typealias (name, _) ->
          let full_path = [String.capitalize name] in
            Some <:expr< $id:ident_with_path _loc full_path ("write_" ^ name)$ b msg >>
      | Message_sum l ->
          let match_case (tag, ns, constr, fields) =
            <:match_case<
            $uid:constr$ msg ->
              $dump_fields ~namespace:(Option.default constr ns) tag fields$ >> in
          let match_cases =
            Ast.mcOr_of_list @@ List.map match_case @@
            List.mapi (fun i (ns, c, fs) -> (i, ns, c, fs)) l
          in Some <:expr< match msg with [ $match_cases$ ] >>
      | Message_subset _ -> None

let wrap_writer _loc opts expr = match get_type_info opts with
    Some { tof; _ } ->
      <:expr<
        try
          let msg = $tof$ msg in
            $expr$
        with [ Extprot.Error.Extprot_error _ as e -> raise e
             | e -> Extprot.Error.conversion_error e]
      >>
  | None -> expr

let add_message_writer ~export bindings msgname mexpr opts c =
  let _loc = Loc.mk "<generated code @ add_message_writer>" in
  let llrec = Gencode.low_level_msg_def bindings msgname mexpr in
    match Option.map (wrap_writer _loc opts) @@ write_message msgname llrec with
      | None -> c
      | Some write_expr ->
          let writer =
            <:str_item< value $lid:"write_" ^ msgname$ b msg = $write_expr$;
                        value write = $lid:"write_" ^ msgname$; >>
          in
            { c with
                c_writer = Some writer;
                c_sig_writer = begin
                  only_if_export opts export @@
                  Printf.sprintf
                          "val write_%s : %s Extprot.Conv.writer\n\
                           val write : %s Extprot.Conv.writer\n"
                          msgname msgname msgname
                end
             }

let msgdecl_generators : (string * _ msgdecl_generator) list =
  [
    "reader", add_message_reader;
    "io_reader", add_message_io_reader;
    "writer", add_message_writer;
    "pretty_printer", Pretty_print.add_msgdecl_pretty_printer;
  ]

let typedecl_generators : (string * _ typedecl_generator) list =
  [
    "pretty_printer", Pretty_print.add_typedecl_pretty_printer;
  ]
