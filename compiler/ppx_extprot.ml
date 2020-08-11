
open Ppxlib

module PT = Protocol_types

let some x = Some x
let is_some = function Some _ -> true | None -> false

let formatter_output f =
  let b   = Buffer.create 13 in
  let fmt = Format.formatter_of_buffer b in
    f fmt;
    Format.pp_print_flush fmt ();
    Buffer.contents b

let string_of_type ty = formatter_output (fun fmt -> Pprintast.core_type fmt ty)
let string_of_expr ty = formatter_output (fun fmt -> Pprintast.expression fmt ty)

let rec flatten_longident_path ~loc = function
  | Ldot (a, b) -> flatten_longident_path ~loc a @ [b]
  | Lident x -> [x]
  | Lapply _ -> Location.raise_errorf ~loc "Invalid longident path"

let extprot_attrs = Hashtbl.create 13

let default_attr =
  Attribute.declare "extprot.default"
    Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun e -> e)

let () =
  Hashtbl.add extprot_attrs "extprot.default" ();
  Hashtbl.add extprot_attrs "default" ()

let default_opt_of_core_type ty =
  match Attribute.get default_attr ty with
    | None -> []
    | Some { pexp_desc = Pexp_constant (Pconst_string (s, _)); _ } ->
        (* we must be especially careful with string literals, because the
         * default option works as in
         *    type string_hohoho = string options "default" = "hohoho"
         * and string_of_expr would turn add extra quotes, turning it into
         *  "\"hohoho\"".
         * *)
        ["default", s]
    | Some { pexp_desc = Pexp_constant (Pconst_integer (s, _)); _ } ->
        (* We drop the letter suffix, which must not be passed to the
         * camlp4-based code generator. *)
        ["default", s]
    | Some e -> ["default", string_of_expr e]

let rec tyexpr_of_core_type ?(ptype_params = []) ty =
  let module Ast_builder = (val Ast_builder.make ty.ptyp_loc) in
  let open Ast_builder in
    match ty.ptyp_desc with

      (* primitive *)
      | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> `Bool (default_opt_of_core_type ty)
      | Ptyp_constr ({ txt = Lident "byte"; _ }, []) -> `Byte (default_opt_of_core_type ty)
      | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> `Int (default_opt_of_core_type ty)
      | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> `Float (default_opt_of_core_type ty)
      | Ptyp_constr ({ txt = Ldot (Lident "Int64", "t"); _ }, []) -> `Long_int (default_opt_of_core_type ty)
      | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> `String (default_opt_of_core_type ty)

      (* complex *)
      | Ptyp_tuple tys ->
          `Tuple (List.map (tyexpr_of_core_type ~ptype_params) tys, [])
      | Ptyp_constr ({ txt = Lident "list"; _ }, [ty]) ->
          `List (tyexpr_of_core_type ~ptype_params ty, [])
      | Ptyp_constr ({ txt = Lident "array"; _ }, [ty]) ->
          `Array (tyexpr_of_core_type ~ptype_params ty, [])

      (* invalid *)
      | Ptyp_constr ({ txt = Lident ("list" | "array"); _ }, ([] | _ :: _))
      | Ptyp_constr ({ txt = Lident ("bool" | "byte" | "int" | "float" | "string"); _ }, _ :: _)
      | Ptyp_constr ({ txt = Ldot (Lident "Int64", "t"); _ }, _ :: _) ->
          Location.raise_errorf ~loc:ty.ptyp_loc "Polymorphic type overrides primitive type"

      (* type application *)
      | Ptyp_constr ({ txt = Lident tyn; _ }, []) -> `App (tyn, [], [])
      | Ptyp_constr ({ txt = Lident tyn; _ }, tys) ->
          `App (tyn, List.map (tyexpr_of_core_type ~ptype_params) tys, [])
      | Ptyp_constr ({ txt = Ldot (path, name); loc; }, []) ->
          `Ext_app (flatten_longident_path ~loc path, name, [], [])

      (* type vars *)
      | Ptyp_var s -> `Type_param (PT.Type_param.type_param_of_string s)

      (* other unsupported types *)
      | Ptyp_constr _
      | Ptyp_any
      | Ptyp_arrow _
      | Ptyp_object _
      | Ptyp_class _
      | Ptyp_alias _
      | Ptyp_variant _
      | Ptyp_poly _
      | Ptyp_package _
      | Ptyp_extension _ ->
          Location.raise_errorf ~loc:ty.ptyp_loc "Invalid type"

let lazy_attr =
  Attribute.declare "extprot.lazy"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    (fun (_ : unit) -> ())

let () =
  Hashtbl.add extprot_attrs "extprot.lazy" ();
  Hashtbl.add extprot_attrs "lazy" ()

let eager_attr =
  Attribute.declare "extprot.eager"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    (fun (_ : unit) -> ())

let () =
  Hashtbl.add extprot_attrs "extprot.eager" ();
  Hashtbl.add extprot_attrs "eager" ()

let autolazy_attr =
  Attribute.declare "extprot.autolazy"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    (fun (_ : unit) -> ())

let () =
  Hashtbl.add extprot_attrs "extprot.autolazy" ();
  Hashtbl.add extprot_attrs "autolazy" ()

let assume_subset_attr =
  Attribute.declare "extprot.assume_subset"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let () =
  Hashtbl.add extprot_attrs "extprot.assume_subset" ();
  Hashtbl.add extprot_attrs "assume_subset" ()

let type_attr =
  Attribute.declare "extprot.type"
    Attribute.Context.type_declaration
    Ast_pattern.(
      pstr @@
      pstr_type recursive
        (type_declaration
           ~name:(string "t")
           ~params:__
           ~cstrs:__
           ~kind:ptype_abstract
           ~private_:public
           ~manifest:(some __) ^::
         nil) ^::
      pstr_value nonrecursive
        (value_binding ~pat:(ppat_var (string "to_t")) ~expr:__ ^:: nil) ^::
      pstr_value nonrecursive
        (value_binding ~pat:(ppat_var (string "from_t")) ~expr:__ ^:: nil) ^::
      alt_option
        (pstr_value nonrecursive
           (value_binding ~pat:(ppat_var (string "default")) ~expr:__ ^:: nil) ^::
         nil)
        nil)
    (fun _params _cstrs manifest to_t_expr from_t_expr default ->
       (manifest, to_t_expr, from_t_expr, default))

let () =
  Hashtbl.add extprot_attrs "extprot.type" ();
  Hashtbl.add extprot_attrs "type" ()

let pp_attr =
  Attribute.declare "extprot.pp"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun e -> e)

let () =
  Hashtbl.add extprot_attrs "extprot.pp" ();
  Hashtbl.add extprot_attrs "pp" ()

type mangled_name_with_attrs = { name  : string; attrs : attributes; }

let hex_alphabet = "0123456789abcdef"

let to_hex s =
  let b = Buffer.create (2 * String.length s) in
    for i = 0 to String.length s - 1 do
      Buffer.add_char b hex_alphabet.[Char.code s.[i] lsr 4];
      Buffer.add_char b hex_alphabet.[Char.code s.[i] land 0xF];
    done;
    Buffer.contents b

let hex_decode_char = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | _   -> failwith "bad hex char"

let from_hex s =
  if String.length s land 1 <> 0 then failwith "bad hex string"
  else
    let b = Buffer.create (String.length s / 2) in
    let i = ref 0 in
      while !i < String.length s do
        Buffer.add_char b
          (Char.chr (hex_decode_char s.[!i] lsl 4 + hex_decode_char s.[!i+1]));
        i := !i + 2
      done;
      Buffer.contents b

let filter_out_extprot_attrs l =
  List.filter
    (fun { attr_name; _ } -> not @@ Hashtbl.mem extprot_attrs attr_name.txt) l

let mangle_name_with_tydecl_attr_info t =
  match filter_out_extprot_attrs @@ t.ptype_attributes with
    | [] -> t.ptype_name.txt
    | attrs ->
        "__EXTPROT_" ^
        (to_hex @@ Marshal.to_string { name = t.ptype_name.txt; attrs; } [])

let unmangle_name s =
  if String.length s < 10 then None
  else
    some @@
    Marshal.from_string
      (from_hex @@ String.sub s 10 (String.length s - 10)) 0

class unmangle_names =
  object(self)
    inherit Ast_traverse.map

    method! type_declaration t =
      match unmangle_name t.ptype_name.txt with
        | None ->
            { t with ptype_kind = self#type_kind t.ptype_kind }
        | Some { name; attrs } ->
            { t with
                ptype_name = { t.ptype_name with txt = name };
                ptype_kind = self#type_kind t.ptype_kind;
                ptype_attributes = attrs;
            }

    method! label_declaration t =
      match unmangle_name t.pld_name.txt with
        | None -> t
        | Some { name; attrs } ->
            { t with
                pld_name = { t.pld_name with txt = name };
                pld_attributes = attrs;
            }
  end

let type_mangled_name_opt_of_tydecl t = match t.ptype_attributes with
  | [] -> []
  | _ -> [ "_ppx.mangled_name", mangle_name_with_tydecl_attr_info t ]

let mangle_name_with_label_info t =
  match filter_out_extprot_attrs @@ t.pld_attributes with
    | [] -> t.pld_name.txt
    | attrs ->
        "__EXTPROT_" ^
        (to_hex @@ Marshal.to_string { name = t.pld_name.txt; attrs; } [])

let field_of_label_decl ?(autolazy = false) pld =
  let module Ast_builder = (val Ast_builder.make pld.pld_loc) in
  let open Ast_builder in
  let evr =
    match autolazy, Attribute.get lazy_attr pld, Attribute.get eager_attr pld with
      | _, Some _, Some _ ->
          Location.raise_errorf ~loc:pld.pld_loc
            "Only one of [@@eager] or [@@lazy] allowed"
      | _, Some _, None -> `Lazy
      | _, None, Some _ -> `Eager
      | true, None, None -> `Auto
      | false, None, None -> `Eager in

  let fopts = match pld.pld_attributes with
    | [] -> []
    | _ -> [ "_ppx.mangled_name", mangle_name_with_label_info pld ]
  in
    (pld.pld_name.txt,
     (match pld.pld_mutable with Immutable -> false | Mutable -> true),
     evr, fopts, tyexpr_of_core_type pld.pld_type)

let extract_type_param (ty, _) =
  match ty.ptyp_desc with
    | Ptyp_var s -> PT.Type_param.type_param_of_string s
    | _ -> Location.raise_errorf ~loc:ty.ptyp_loc "Type param is not a type var."

let rev_decls = ref []
let fieldmod = ref "Extprot.Field"
let assume_subsets = ref ""

let is_record_type name =
  List.exists
    (function
      | PT.Message_decl (n, _, _, _) -> n = name
      | PT.Type_decl (n, _, `Record _, _) -> n = name
      | PT.Type_decl _ -> false )
    !rev_decls

let type_equals_opt_of_tydecl t =
  (* quick reminder:
   *
     type t                     (abstract, no manifest)
     type t = T0                (abstract, manifest=T0)
     type t = C of T | ...      (variant,  no manifest)
     type t = T0 = C of T | ... (variant,  manifest=T0)
     type t = {l: T; ...}       (record,   no manifest)
     type t = T0 = {l : T; ...} (record,   manifest=T0)
     type t = ..                (open,     no manifest)
   * *)
  match t.ptype_manifest, t.ptype_kind with
    | None, _ -> []
    | Some ({ ptyp_desc = Ptyp_constr ({ txt = path; _ }, _); _ } as ty),
      (Ptype_variant _ | Ptype_record _) ->
        let path = flatten_longident_path ~loc:ty.ptyp_loc path in
          [ "ocaml.type_equals", String.concat "." path ]
    | Some { ptyp_loc; _ }, _ -> []

let type_opt_of_tydecl t =
  match Attribute.get type_attr t with
    | None -> []
    | Some (manifest, to_t_expr, from_t_expr, default) ->
        List.filter_map (function (k, None) -> None | (k, Some v) -> Some (k, v))
          [
            "ocaml._type__type", some @@ string_of_type manifest;

            "ocaml._type__to_t", some @@ string_of_expr to_t_expr;

            "ocaml._type__from_t", some @@ string_of_expr from_t_expr;

            "ocaml._type__default", Option.map string_of_expr default;
        ]

let pp_opt_of_tydecl t =
  match Attribute.get pp_attr t with
    | None -> []
    | Some e -> [ "ocaml.pp", string_of_expr e ]

(* force_message: set when using  extprot.message  to (1) reject
 * anything that is not a monomorphic record type and (2) register the
 * declaration as a message declaration whose (de)serialization functions
 * are exported *)
let decl_of_ty ~export ~force_message ~loc tydecl =
  let module Ast_builder = (val Ast_builder.make loc) in
  let open Ast_builder in
    match tydecl with
      | { ptype_name = { txt = name; _ };
          ptype_params = []; ptype_cstrs = [];
          ptype_kind = Ptype_record fs; _ } ->
          let autolazy = is_some @@ Attribute.get autolazy_attr tydecl in
          let mexpr = `Message_record (List.map (field_of_label_decl ~autolazy) fs) in
            PT.Message_decl
              (name, mexpr, (if export then Export_YES else Export_NO),
               List.concat
                 [ type_equals_opt_of_tydecl tydecl;
                   type_opt_of_tydecl tydecl;
                   type_mangled_name_opt_of_tydecl tydecl;
                 ])

      (* special-case Int64.t, which is NOT a message alias *)
      | { ptype_name = { txt = name; _ };
          ptype_params = []; ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_loc;
          ptype_manifest =
            Some ({ ptyp_desc = Ptyp_constr ({ txt = (Ldot (Lident "Int64", "t")); _ }, []); _ } as ty);
          _ } ->
          PT.Type_decl
            (name, [], `Long_int (default_opt_of_core_type ty),
             List.concat
               [type_opt_of_tydecl tydecl;
                pp_opt_of_tydecl tydecl;
                type_mangled_name_opt_of_tydecl tydecl;
               ])

      | { ptype_name = { txt = name; _ };
          ptype_params = []; ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_loc;
          ptype_manifest =
            Some { ptyp_desc = Ptyp_constr ({ txt = (Ldot _ as path); _ }, []); _ };
          _ } ->

          let opath, oname = match List.rev @@ flatten_longident_path ~loc:ptype_loc path with
            | name :: rev_path -> (List.rev rev_path, name)
            | [] -> Location.raise_errorf ~loc:ptype_loc "Invalid longindent path" in

          let mexpr = `Message_alias (opath, oname) in
            PT.Message_decl
              (name, mexpr, (if export then Export_YES else Export_NO),
               (type_mangled_name_opt_of_tydecl tydecl;))

      | { ptype_name = { txt = name; _ };
          ptype_params = []; ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_manifest = Some cty; ptype_loc; _ } -> begin
          match tyexpr_of_core_type cty with
            | `App (tyn, tys, opts) when is_record_type tyn ->
                let mexpr = `Message_app (tyn, tys, opts) in
                  PT.Message_decl
                    (name, mexpr, Export_YES,
                     (type_mangled_name_opt_of_tydecl tydecl;))
            | _ when not force_message ->
                PT.Type_decl
                  (name, [], (tyexpr_of_core_type cty :> PT.type_expr),
                   List.concat
                     [
                       type_equals_opt_of_tydecl tydecl;
                       type_opt_of_tydecl tydecl;
                       pp_opt_of_tydecl tydecl;
                       type_mangled_name_opt_of_tydecl tydecl;
                     ])
            | _ ->
                Location.raise_errorf ~loc:ptype_loc
                  "Message declaration expected (disjoin union or record type)"
        end

      | { ptype_name = { txt = name; _ };
          ptype_params; ptype_cstrs = [];
          ptype_kind = Ptype_variant constrs;
          ptype_loc;
          _ } -> begin

          (* check whether all constructors are records or tuples with
           * 1 type argument that is actually a record type application
           * *)
          match
            List.for_all
              (fun cd ->
                 match cd.pcd_args with
                   | Pcstr_record _ -> true
                   | Pcstr_tuple
                       [{ptyp_desc = Ptyp_constr ({txt = Lident name; _ }, _)}] ->
                       is_record_type name
                   | Pcstr_tuple _ -> false)
              constrs
          with
            | true when ptype_params = [] ->
                (* all constructors correspond to a record,
                 * treat this as a disjoint union of messages *)
                let branches =
                  List.map
                    (fun cd ->
                       match cd.pcd_args with
                         | Pcstr_record fs ->
                             (cd.pcd_name.txt,
                              `Message_record (List.map field_of_label_decl fs))
                         | Pcstr_tuple
                             [{ptyp_desc = Ptyp_constr ({txt = Lident name; _ }, params)}] -> begin
                             match
                               List.find_map
                                 (function
                                   | PT.Type_decl (n, _, `Record (r, _opts), _) ->
                                       if n = name then Some r.PT.record_name
                                       else None
                                   | PT.Message_decl (n, _, _, _) ->
                                       if n = name then Some n
                                       else None
                                   | _ -> None)
                                 !rev_decls
                             with
                               | None ->
                                   Location.raise_errorf ~loc:cd.pcd_loc
                                     "Unknown record type"
                               | Some recname ->
                                   (cd.pcd_name.txt,
                                    `Message_app
                                      (recname,
                                       List.map tyexpr_of_core_type params, []))
                           end
                         | Pcstr_tuple _ ->
                             Location.raise_errorf ~loc:cd.pcd_loc
                               "Invalid type in message union branch")
                    constrs in
                let mexpr = `Message_sum branches in
                  PT.Message_decl
                    (name, mexpr, Export_YES,
                     (type_mangled_name_opt_of_tydecl tydecl))

            | _ when not force_message ->
                (* some constructor does not correspond to a record,
                 * treat this as a regular sum type *)
                let constructors =
                  List.map
                    (fun cd ->
                       match cd.pcd_args with
                         | Pcstr_record _ ->
                             Location.raise_errorf ~loc:cd.pcd_loc
                               "Unsupported inline record"
                         | Pcstr_tuple [] -> `Constant (cd.pcd_name.txt, [])
                         | Pcstr_tuple tys ->
                             `Non_constant
                               (cd.pcd_name.txt, [],
                                List.map (tyexpr_of_core_type ~ptype_params) tys))
                    constrs
                in
                  PT.Type_decl
                    (name, List.map extract_type_param ptype_params,
                     `Sum ({ PT.type_name = name; constructors }, []),
                     List.concat
                       [ type_equals_opt_of_tydecl tydecl;
                         type_opt_of_tydecl tydecl;
                         pp_opt_of_tydecl tydecl;
                         type_mangled_name_opt_of_tydecl tydecl;
                       ])

            | _ (* force_message *) ->
                Location.raise_errorf ~loc:ptype_loc
                  "Message declaration expected (disjoin union or record type)"
        end

      | _ when force_message ->
          Location.raise_errorf ~loc:tydecl.ptype_loc
            "Message declaration expected (disjoin union or record type)"

      | { ptype_name = { txt = name; _ };
          ptype_params; ptype_cstrs = [];
          ptype_kind = Ptype_record fs; _ } ->
          let autolazy = is_some @@ Attribute.get autolazy_attr tydecl in
          let r =
            { PT.record_name = name;
              record_fields = List.map (field_of_label_decl ~autolazy) fs;
            }
          in
            PT.Type_decl
              (name, List.map extract_type_param ptype_params, `Record (r, []),
               List.concat
                 [ type_equals_opt_of_tydecl tydecl;
                   type_opt_of_tydecl tydecl;
                   pp_opt_of_tydecl tydecl;
                   type_mangled_name_opt_of_tydecl tydecl;
                 ])

      | { ptype_name = { txt = name; _ };
          ptype_params; ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_manifest = Some cty; _ } ->
          PT.Type_decl
            (name, List.map extract_type_param ptype_params,
             (tyexpr_of_core_type ~ptype_params cty :> PT.type_expr),
             List.concat
               [ type_equals_opt_of_tydecl tydecl;
                 type_opt_of_tydecl tydecl;
                 pp_opt_of_tydecl tydecl;
                 type_mangled_name_opt_of_tydecl tydecl;
               ])

      | _ ->
          Location.raise_errorf ~loc "Expected a record or simple type definition"

module G = Gencode.Make(Gen_OCaml)

type 'a gencode =
  | Gen_impl : Parsetree.structure_item gencode
  | Gen_sig : Parsetree.signature_item gencode

let register_decl_and_gencode (type a) (which : a gencode) ?(assume_subset = false) ~loc decl : a =
  rev_decls := decl :: !rev_decls;
  let decls = List.rev !rev_decls in
  let implem, signature =
    try
      G.generate_code
        ~global_opts:begin
          List.concat @@
          [
            ["field-module", !fieldmod];
            begin match !assume_subsets, assume_subset, decl with
              | "", false, _ -> []
              | s, false, _ -> ["assume_subsets", s]
              | "", true, (PT.Message_decl (n, _, _, _) | PT.Type_decl (n, _, _, _)) ->
                  ["assume_subsets", String.capitalize_ascii n]
              | s, true, (PT.Message_decl (n, _, _, _) | PT.Type_decl (n, _, _, _)) ->
                  ["assume_subsets", s ^ "," ^ String.capitalize_ascii n]
            end
          ]
        end
        ~width:100
        (Gencode.collect_bindings decls) [PT.Decl decl]
    with exn ->
      Location.raise_errorf ~loc
        "Extprot codegen error: %s" (Printexc.to_string exn)
  in
    match which with
      | Gen_impl ->
          begin match Parse.implementation @@ Lexing.from_string implem with
            | _ :: str :: _ ->
                (* skip the first element: EXTPROT_FIELD____ def *)
                (new unmangle_names)#structure_item str
            | _ -> Location.raise_errorf ~loc "Codegen error"
          end
      | Gen_sig ->
          begin match Parse.interface @@ Lexing.from_string signature with
            | str :: _ ->
                (new unmangle_names)#signature_item str
            | [] -> Location.raise_errorf ~loc "Codegen error"
          end

let must_assume_subset ty = is_some @@ Attribute.get assume_subset_attr ty

let expand_function which ~force_message ~export ~loc ~path ty =
  let module Ast_builder = (val Ast_builder.make loc) in
  let open Ast_builder in
    register_decl_and_gencode ~assume_subset:(must_assume_subset ty) which ~loc @@
    decl_of_ty ~force_message ~export ~loc ty

let include_attr =
  Attribute.declare "extprot.include"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let () =
  Hashtbl.add extprot_attrs "extprot.include" ();
  Hashtbl.add extprot_attrs "include" ()

let exclude_attr =
  Attribute.declare "extprot.exclude"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let () =
  Hashtbl.add extprot_attrs "extprot.exclude" ();
  Hashtbl.add extprot_attrs "exclude" ()

let lazy_expr_attr =
  Attribute.declare "extprot.lazy"
    Attribute.Context.expression
    Ast_pattern.(pstr nil)
    (fun (_ : unit) -> ())

let eager_expr_attr =
  Attribute.declare "extprot.eager"
    Attribute.Context.expression
    Ast_pattern.(pstr nil)
    (fun (_ : unit) -> ())

let evr_of_exp e =
  match Attribute.get lazy_expr_attr e, Attribute.get eager_expr_attr e with
    | None, None -> None
    | Some _, Some _ ->
        Location.raise_errorf ~loc:e.pexp_loc
          "Only one of [@@eager] or [@@lazy] allowed"
    | Some _, None -> Some `Lazy
    | None, Some _ -> Some `Eager

let subset_field_with_ascription e ty =
  match e.pexp_desc, ty with
    | Pexp_ident { txt = Lident n; _ }, ty ->
        (n, (Some (tyexpr_of_core_type ty), evr_of_exp e))
    | _ ->
        Location.raise_errorf
          ~loc:e.pexp_loc  "Expected a lowercase identifier"

let split_lid_tuple ?(allow_ascription = false) = function
  | { pexp_desc = Pexp_ident { txt = (Lident s); _ }; _ } as e ->
        [s, (None, evr_of_exp e)]
  | [%expr ([%e? e] : [%t? ty])] when allow_ascription ->
      [ subset_field_with_ascription e ty ]
  | { pexp_desc = Pexp_tuple l; _ } ->
      List.map
        (function
          | { pexp_desc = Pexp_ident { txt = (Lident n); _ }; _ } as e ->
              (n, (None, evr_of_exp e))
          | [%expr ([%e? e] : [%t? ty])] when allow_ascription -> begin
              subset_field_with_ascription e ty
            end
          | { pexp_loc; _ } ->
              Location.raise_errorf ~loc:pexp_loc
                "Expected a lowercase identifier")
        l
  | { pexp_loc; _ } ->
      Location.raise_errorf ~loc:pexp_loc
        "Expected a tuple like   field1, field2, field3 "

let subset_decl_of_includes ~name ~orig inc =
  let fields = split_lid_tuple ~allow_ascription:true inc in
    PT.Message_decl
      (name, `Message_subset (orig, fields, `Include), PT.Export_YES, [])

let subset_decl_of_excludes ~name ~orig inc =
  let fields = split_lid_tuple inc in
    PT.Message_decl
      (name, `Message_subset (orig, fields, `Exclude), PT.Export_YES, [])

let expand_subset which ~loc ~path tydecl =
  let module Ast_builder = (val Ast_builder.make loc) in
  let open Ast_builder in
    match tydecl with
      | { ptype_name = { txt = name; _ };
          ptype_params = []; ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_loc;
          ptype_manifest =
            Some { ptyp_desc = Ptyp_constr ({ txt = Lident orig; _ }, _); _ };
          _
        } ->
          let includes = Attribute.get include_attr tydecl in
          let excludes = Attribute.get exclude_attr tydecl in
          let decl =
            match includes, excludes with
              | Some inc, None -> subset_decl_of_includes ~name ~orig inc
              | None, Some exc -> subset_decl_of_excludes ~name ~orig exc
              | None, None ->
                  Location.raise_errorf ~loc
                    "Need either [@@include a,b,c] or [@@include a,b,c] attribute"
              | Some _, Some _ ->
                  Location.raise_errorf ~loc
                    "Only one of [@@include a,b,c] or [@@include a,b,c] allowed"
          in
            register_decl_and_gencode which ~loc decl
      | _ ->
          Location.raise_errorf ~loc:tydecl.ptype_loc
            "Expected a monomorphic type definition of the form  type%%subset foo = bar"

let extprot_ext =
  Ppxlib.Extension.declare
    "extprot"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(
      pstr @@
      pstr_type __ (__ ^:: nil) ^::
      nil)
    (fun ~loc ~path _ ty ->
       expand_function Gen_impl ~force_message:false ~export:false ~loc ~path ty)

let extprot_ext' =
  Ppxlib.Extension.declare_inline
    "extprot"
    Ppxlib.Extension.Context.signature_item
    Ppxlib.Ast_pattern.(
      psig @@
      psig_type __ (__ ^:: nil) ^::
      nil)
    (fun ~loc ~path _ ty ->
       [expand_function Gen_sig ~force_message:false ~export:false ~loc ~path ty])

let message_ext =
  Ppxlib.Extension.declare
    "extprot.message"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(
      pstr @@
      pstr_type __ (__ ^:: nil) ^::
      nil)
    (fun ~loc ~path _ ty ->
       expand_function Gen_impl ~force_message:true ~export:true ~loc ~path ty)

let message_ext' =
  Ppxlib.Extension.declare
    "extprot.message"
    Ppxlib.Extension.Context.signature_item
    Ppxlib.Ast_pattern.(
      psig @@
      psig_type __ (__ ^:: nil) ^::
      nil)
    (fun ~loc ~path _ ty ->
       expand_function Gen_sig ~force_message:true ~export:true ~loc ~path ty)

let subset_ext =
  Ppxlib.Extension.declare
    "extprot.subset"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(
      pstr @@
      pstr_type __ (__ ^:: nil) ^::
      nil)
    (fun ~loc ~path _ ty -> expand_subset Gen_impl ~loc ~path ty)

let subset_ext' =
  Ppxlib.Extension.declare
    "extprot.subset"
    Ppxlib.Extension.Context.signature_item
    Ppxlib.Ast_pattern.(
      psig @@
      psig_type __ (__ ^:: nil) ^::
      nil)
    (fun ~loc ~path _ ty -> expand_subset Gen_sig ~loc ~path ty)

let fieldmod_ext =
  Ppxlib.Extension.declare
    "extprot.fieldmod"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(single_expr_payload @@ pexp_construct __ none)
    (fun ~loc ~path longident ->
       let module Ast_builder = (val Ast_builder.make loc) in
       let open Ast_builder in
       let m = pmod_ident { txt = longident; loc } in
         [%stri module EXTPROT_FIELD____ = [%m m]])

let assume_subsets_ext =
  Ppxlib.Extension.declare_inline
    "extprot.assume_subsets"
    Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(pstr nil)
    (fun ~loc ~path -> assume_subsets := "all"; [])

let fieldmod_ext' =
  Ppxlib.Extension.declare_inline
    "extprot.fieldmod"
    Ppxlib.Extension.Context.signature_item
    Ppxlib.Ast_pattern.(single_expr_payload @@ pexp_construct __ none)
    (fun ~loc ~path longident ->
       fieldmod := String.concat "." @@ flatten_longident_path ~loc longident;
       [])

let rule1  = Ppxlib.Context_free.Rule.extension extprot_ext
let rule1' = Ppxlib.Context_free.Rule.extension extprot_ext'
let rule2  = Ppxlib.Context_free.Rule.extension message_ext
let rule2' = Ppxlib.Context_free.Rule.extension message_ext'
let rule3  = Ppxlib.Context_free.Rule.extension subset_ext
let rule3' = Ppxlib.Context_free.Rule.extension subset_ext'
let rule4  = Ppxlib.Context_free.Rule.extension fieldmod_ext
let rule4' = Ppxlib.Context_free.Rule.extension fieldmod_ext'
let rule5  = Ppxlib.Context_free.Rule.extension assume_subsets_ext

let () =
  Ppxlib.Driver.register_transformation
    ~rules:[rule1; rule1'; rule2; rule2'; rule3; rule3';
            rule4; rule4'; rule5 ]
    "extprot"

let () = Ppxlib.Driver.standalone ()
